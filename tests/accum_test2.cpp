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
#include <LPE/accum.h>
#include <cassert>
#include <iostream>

using namespace LPE;

struct RayEvent
{
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
  std::vector<int> expected;    // What aovs are expected for this path

  TestPath(std::vector<RayEvent> e, std::vector<int> ex) :
      events{std::move(e)},
      expected{std::move(ex)}
  {
  }
};

// This is a fake AOV implementation. It will just keep track
// of what test cases wrote to it
class MyAov : public Aov
{
public:
  std::string name;

  std::vector<bool> m_expected;
  std::vector<bool> m_received;

  MyAov(const std::vector<TestPath> &tests, int id, std::string name) :
      name{std::move(name)}
  {
    // Init for the test case array. For each test case set a bool
    // in m_expected marking wether this AOV should get soem color
    // from that test or not.
    for (size_t i = 0; i < tests.size(); ++i) {
      const auto &test = tests[i];
      m_expected.push_back(false);
      for (const auto expected : test.expected) {
        if (expected == id) {
          m_expected[i] = true;
        }
      }
    }

    m_received.resize(m_expected.size());
  }
  virtual ~MyAov()
  {
  }

  virtual void write(void *flush_data, Color3 &color, float alpha,
      bool has_color, bool has_alpha)
  {
    // our custom argument to write is the rule's number so we
    // can check what. But you normally would pass information
    // about the pixel.
    long int testno = (long int)flush_data;
    if (has_color && color.x > 0)
      // only mrk with true if there is a positive color present
      m_received[testno] = true;
    else
      m_received[testno] = false;
  }

  bool check() const
  {
    return m_expected == m_received;
  }
};

// Simulate the tracing of a path with the accumulator
void simulate(
    Accumulator &accum, const std::vector<RayEvent> &events, int testno)
{
  accum.begin();
  accum.pushState();

  for (const auto &e : events) {
    accum.move(e.eventType);
    if (e.scatteringType) {
      accum.move(e.scatteringType);
    }
    if (e.object) {
      accum.move(e.object);
    }
    accum.move(Labels::STOP);
  }

  // Here is were we have reached a light, accumulate color
  accum.accum(Color3(1, 1, 1));
  // Restore state and flush
  accum.popState();
  accum.end((void *)(long int)testno);
}

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
  naovs
};

struct AovDesc
{
  const char *name;
  const char *lpe;
};

// clang-format off

const AovDesc aovDescs[] = 
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
  {"custom",        "CDY+U"}
};

// clang-format on

int main()
{
  TestPath test1{
      {{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, specular, nocaustic}};

  TestPath test2{{{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"R", "G"},
                     {"L", "_", "1"}},
      {}};

  TestPath test3{{{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"R", "D"},
                     {"L", "_", "1"}},
      {beauty, specular, diffuse2_3, nocaustic}};

  TestPath test4{{{"C", "_"}, {"R", "G"}, {"R", "D"}, {"R", "G"}, {"R", "D"},
                     {"R", "G"}, {"R", "D"}, {"L", "_", "1"}},
      {}};

  TestPath test5{{{"C", "_"}, {"R", "G"}, {"R", "D"}, {"R", "G"}, {"R", "D"},
                     {"L", "_", "1"}},
      {nocaustic}};

  TestPath test6{{{"C", "_"}, {"R", "D"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, diffuse, diffuse2_3, nocaustic}};

  TestPath test7{
      {{"C", "_"}, {"R", "D"}, {"R", "S"}, {"R", "D"}, {"L", "_", "1"}},
      {nocaustic}};

  TestPath test8{
      {{"C", "_"}, {"R", "D"}, {"T", "s"}, {"L", "_"}}, {transpshadow}};

  TestPath test9{
      {{"C", "_"}, {"T", "S"}, {"T", "S"}, {"R", "D"}, {"L", "_", "3"}},
      {beauty, specular, light3, nocaustic}};

  TestPath test10{{{"C", "_"}, {"R", "D", "1"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, diffuse, diffuse2_3, object_1, nocaustic}};

  TestPath test11{
      {{"C", "_"}, {"R", "S"}, {"R", "D"}, {"R", "G"}, {"L", "_", "1"}}, {}};

  TestPath test12{{{"C", "_"}, {"R", "S"}, {"R", "D"}, {"L", "_", "1"}},
      {beauty, specular, reflections, nocaustic}};

  TestPath test13{
      {{"C", "_"}, {"R", "D"}, {"R", "Y"}, {"T", "Y"}, {"U", "_"}}, {custom}};

  std::vector<TestPath> test = {test1, test2, test3, test4, test5, test6, test7,
      test8, test9, test10, test11, test12, test13};

  // Create our fake testing AOV's
  std::vector<MyAov> aovs;
  for (int i = 0; i < naovs; ++i)
    aovs.emplace_back(test, i, aovDescs[i].name);

  // Create the automata and add the rules
  AccumAutomata automata;

  automata.addEventType(std::string("U"));
  automata.addScatteringType(std::string("Y"));

  for (size_t i = 0; i < aovs.size(); ++i) {
    assert(automata.addRule(aovDescs[i].lpe, int(i)));
  }

  automata.compile();

  // now create the accumulator
  Accumulator accum(&automata);

  // and set the AOV's for each id (beauty, diffuse2_3, etc ...)
  for (int i = 0; i < naovs; ++i)
    accum.setAov(i, &aovs[i], false, false);

  // do the simulation for each test case
  for (size_t i = 0; i < test.size(); ++i)
    simulate(accum, test[i].events, i);

  // And check. We unroll this loop for boost to give us a useful
  // error in case they fail
  bool fail = false;
  for (const auto &aov : aovs) {
    if (!aov.check()) {
      std::cerr << "Check failed for aov " << aov.name << std::endl;
      for (size_t i = 0; i < aov.m_expected.size(); ++i) {
        if (aov.m_expected[i] != aov.m_received[i]) {
          std::cerr << " - failed for path " << (i + 1) << " expected "
                    << int(aov.m_expected[i]) << " and received "
                    << int(aov.m_received[i]) << std::endl;
        }
      }
      fail = true;
    }
  }

  if (!fail) {
    std::cout << "Light expressions check OK" << std::endl;
  }
  return 0;
}
