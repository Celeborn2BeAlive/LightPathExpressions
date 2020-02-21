/*
Copyright (c) 2009-2010 Sony Pictures Imageworks Inc., et al.
All Rights Reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer in the
  documentation and/or other materials provided with the distribution.
* Neither the name of Sony Pictures Imageworks nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
 * Terminology:
 * - DF Automata: Deterministic Finite Automata
 * - NDF Automata: Non Deterministic Finite Automata
 */
#include "lpeparse.h"
#include "lpexp.h"
#include <LPE/optautomata.h>

#include <cassert>
#include <fstream>
#include <iostream>
#include <stack>

#include "json.hpp"

using namespace LPE;

struct RayEvent
{
  // This are strings
  const char *eventType;
  const char *scatteringType;
  const char *object;

  RayEvent(const char *e, const char *s, const char *o = nullptr) :
      eventType{e},
      scatteringType{s},
      object{o}
  {
  }
};

struct TestPath
{
  std::vector<RayEvent> events; // Array of events of the path
  std::vector<size_t>
      expectedLPEs; // Indices of LPEs that are expected to match this path

  TestPath(std::vector<RayEvent> e, std::vector<size_t> ex) :
      events{std::move(e)},
      expectedLPEs{std::move(ex)}
  {
  }
};

class LpeAov
{
public:
  std::string name;

  // For each path, we have a boolean telling us if it is expected to be
  // recognized by the LPE of this AOV
  std::vector<bool> expected;

  // Will be filled during "path tracing"
  std::vector<bool> received;

  LpeAov(int aovIdx, std::string aovName, const std::vector<TestPath> &paths) :
      name{std::move(aovName)},
      expected(paths.size(), false),
      received(paths.size(), false)
  {
    for (size_t i = 0; i < paths.size(); ++i) {
      const auto &path = paths[i];
      for (const auto expectedIndex : path.expectedLPEs) {
        if (expectedIndex == aovIdx) {
          expected[i] = true;
        }
      }
    }

    received.resize(expected.size());
  }

  bool check() const
  {
    return expected == received;
  }
};

// Some constants to avoid refering to AOV's by number
enum {
  beauty,
  diffuse2_3,
  light3,
  object_1,
  specular,
  diffuse,
  transpshadow,
  reflections,
  nocaustic,
  custom,
  my_light,
  my_cam,
  LPE_COUNT
};

struct LpeDesc
{
  const char *name;
  const char *lpe;
};

// clang-format off

const LpeDesc aovDescs[] = 
{
  {"beauty",        "C[SG]*D*<L..>"},
  {"diffuse2_3",    "C[SG]*D{2,3}L"},
  {"light3",        "C[SG]*D*<L.'3'>"},
  {"object_1",      "C[SG]*<.D'1'>D*L"},
  {"specular",      "C<.[SG]>+D*L"},
  {"diffuse",       "CD+L"},
  {"transpshadow",  "CD+<Ts>L"},
  {"reflections",   "C<R[^D]>+D*L"},
  {"nocaustic",     "C([SG]*D){1,2}L"},
  {"custom",        "CDY+U"},
  {"my_light",      "C[SG]*D*<L.'my_light'>"},
  {"my_cam",        "<'my_cam'.>.*"}
};
static_assert(std::size(aovDescs) == LPE_COUNT);

// clang-format on

using json = nlohmann::json;

struct JsonSerializerVisitor : public lpexp::LPexpVisitor
{
  json root;
  std::stack<json> parent;

  bool enter(const lpexp::Cat &exp) override
  {
    parent.emplace(json{{"type", "Cat"}, {"children", json::array()}});
    return true;
  }

  bool enter(const lpexp::Symbol &exp) override
  {
    parent.emplace(json{{"type", "Symbol"}, {"value", exp.m_sym}});
    return true;
  }

  bool enter(const lpexp::Wildexp &exp) override
  {
    const auto &wildcard = exp.m_wildcard;
    parent.emplace(json{{"type", "Wildexp"}, {"minus", wildcard.m_minus}});
    return true;
  }

  bool enter(const lpexp::Orlist &exp) override
  {
    parent.emplace(json{{"type", "Or"}, {"children", json::array()}});
    return true;
  }

  bool enter(const lpexp::Repeat &exp) override
  {
    parent.emplace(json{{"type", "Repeat"}, {"children", json::array()}});
    return true;
  }

  bool enter(const lpexp::NRepeat &exp) override
  {
    parent.emplace(json{{"type", "NRepeat"}, {"min", exp.m_min},
        {"max", exp.m_max}, {"children", json::array()}});
    return true;
  }

  void leave(const lpexp::LPexp &exp)
  {
    const auto object = parent.top();
    parent.pop();
    if (!parent.empty()) {
      parent.top()["children"].emplace_back(object);
    } else {
      root = object;
    }
  }
};

int main()
{
  TestPath path1{
      {{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, specular, nocaustic}};

  TestPath path2{{{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"R", "G"},
                     {"L", "_", "1"}},
      {}};

  TestPath path3{{{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"R", "D"},
                     {"L", "_", "1"}},
      {beauty, specular, diffuse2_3, nocaustic}};

  TestPath path4{{{"C", "_"}, {"R", "G"}, {"R", "D"}, {"R", "G"}, {"R", "D"},
                     {"R", "G"}, {"R", "D"}, {"L", "_", "1"}},
      {}};

  TestPath path5{{{"C", "_"}, {"R", "G"}, {"R", "D"}, {"R", "G"}, {"R", "D"},
                     {"L", "_", "1"}},
      {nocaustic}};

  TestPath path6{{{"C", "_"}, {"R", "D"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, diffuse, diffuse2_3, nocaustic}};

  TestPath path7{
      {{"C", "_"}, {"R", "D"}, {"R", "S"}, {"R", "D"}, {"L", "_", "1"}},
      {nocaustic}};

  TestPath path8{
      {{"C", "_"}, {"R", "D"}, {"T", "s"}, {"L", "_"}}, {transpshadow}};

  TestPath path9{
      {{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"L", "_", "3"}},
      {beauty, specular, light3, nocaustic}};

  TestPath path10{{{"C", "_"}, {"R", "D", "1"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, diffuse, diffuse2_3, object_1, nocaustic}};

  TestPath path11{
      {{"C", "_"}, {"R", "S"}, {"R", "D"}, {"R", "G"}, {"L", "_", "1"}}, {}};

  TestPath path12{{{"C", "_"}, {"R", "S"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, specular, reflections, nocaustic}};

  TestPath path13{
      {{"C", "_"}, {"R", "D"}, {"R", "Y"}, {"T", "Y"}, {"U", "_"}}, {custom}};

  TestPath path14{
      {{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"L", "_", "my_light"}},
      {beauty, specular, my_light, nocaustic}};

  TestPath path15{{{"my_cam", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"},
                      {"L", "_", "my_light"}},
      {my_cam}};

  std::vector<TestPath> paths = {path1, path2, path3, path4, path5, path6,
      path7, path8, path9, path10, path11, path12, path13, path14, path15};

  // Create our fake testing AOV's
  std::vector<LpeAov> aovs;
  for (int i = 0; i < LPE_COUNT; ++i)
    aovs.emplace_back(i, aovDescs[i].name, paths);

  std::vector<std::string> userEvents = {"U"};
  std::vector<std::string> userScatterings = {"Y"};

  std::vector<LPexp *> lpeAsts(aovs.size());

  json lpexpJson;

  for (size_t i = 0; i < aovs.size(); ++i) {
    Parser parser(&userEvents, &userScatterings);
    lpeAsts[i] = parser.parse(aovDescs[i].lpe);

    if (parser.error()) {
      std::cerr << "[pathexp] Parse error" << parser.getErrorMsg()
                << " at char " << parser.getErrorPos() << std::endl;
      std::exit(-1);
    }

    JsonSerializerVisitor v;
    lpeAsts[i]->accept(v);

    json object;
    object["name"] = aovDescs[i].name;
    object["lpe"] = aovDescs[i].lpe;
    object["tree"] = v.root;
    lpexpJson[aovDescs[i].name] = object;
  }

  {
    std::ofstream jsonOut("lpe.json");
    jsonOut << lpexpJson.dump(2);
  }

  NdfAutomata ndfautomata;
  for (size_t i = 0; i < lpeAsts.size(); ++i) {
    generateAutomata(ndfautomata, *lpeAsts[i], i + 1);
  }
  DfAutomata dfautomata;
  ndfautoToDfauto(ndfautomata, dfautomata);

  DfOptimizedAutomata dfoptautomata;
  dfoptautomata.compileFrom(dfautomata);

  // do the simulation for each test case
  for (size_t pathIdx = 0; pathIdx < paths.size(); ++pathIdx) {
    auto state = 0;

    for (const auto &e : paths[pathIdx].events) {
      state = dfoptautomata.getTransition(state, e.eventType);

      if (state < 0) {
        break;
      }
      if (e.scatteringType) {
        state = dfoptautomata.getTransition(state, e.scatteringType);
        if (state < 0) {
          break;
        }
      }
      if (e.object) {
        state = dfoptautomata.getTransition(state, e.object);
        if (state < 0) {
          break;
        }
      }
      state = dfoptautomata.getTransition(state, Labels::STOP);
      if (state < 0) {
        break;
      }
    }

    if (state >= 0) {
      unsigned int nrules = 0;
      unsigned int const *const rules = dfoptautomata.getRules(state, nrules);
      // Iterate the vector
      for (auto k = 0u; k < nrules; ++k) {
        // This is where we should accumulate the color sample
        const auto outputIdx = rules[k] - 1;
        aovs[outputIdx].received[pathIdx] = true;
      }
    }
  }

  // And check. We unroll this loop for boost to give us a useful
  // error in case they fail
  bool fail = false;
  for (const auto &aov : aovs) {
    if (!aov.check()) {
      std::cerr << "Check failed for aov " << aov.name << std::endl;
      for (size_t i = 0; i < aov.expected.size(); ++i) {
        if (aov.expected[i] != aov.received[i]) {
          std::cerr << " - failed for path " << (i + 1) << " expected "
                    << int(aov.expected[i]) << " and received "
                    << int(aov.received[i]) << std::endl;
        }
      }
      fail = true;
    } else {
      std::cout << "Aov " << aov.name << " OK " << std::endl;
    }
  }

  if (!fail) {
    std::cout << "Light expressions check OK" << std::endl;
  }
  return 0;
}
