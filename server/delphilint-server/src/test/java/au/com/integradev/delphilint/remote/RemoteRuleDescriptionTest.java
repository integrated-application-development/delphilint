/*
 * DelphiLint Server
 * Copyright (C) 2024 Integrated Application Development
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
package au.com.integradev.delphilint.remote;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import org.junit.jupiter.api.Test;

class RemoteRuleDescriptionTest {
  private static class TestDescriptionSection {
    private final String key;
    private final String content;

    public TestDescriptionSection(String key, String content) {
      this.key = key;
      this.content = content;
    }

    public String getKey() {
      return key;
    }

    public String getContent() {
      return content;
    }
  }

  @Test
  void testFromDescriptionSections() {
    var desc =
        RemoteRuleDescription.fromDescriptionSections(
            List.of(
                new TestDescriptionSection("introduction", "foo intro"),
                new TestDescriptionSection("root_cause", "bar root cause"),
                new TestDescriptionSection("how_to_fix", "baz how to fix"),
                new TestDescriptionSection("resources", "flarp resources")),
            TestDescriptionSection::getKey,
            TestDescriptionSection::getContent);

    assertEquals("foo intro", desc.getIntroductionSection());
    assertEquals("bar root cause", desc.getRootCauseSection());
    assertEquals("baz how to fix", desc.getHowToFixSection());
    assertEquals("flarp resources", desc.getResourcesSection());
  }

  @Test
  void testFromDescriptionSectionsEmptySections() {
    var desc =
        RemoteRuleDescription.fromDescriptionSections(
            List.of(
                new TestDescriptionSection("introduction", "foo intro"),
                new TestDescriptionSection("resources", "flarp resources")),
            TestDescriptionSection::getKey,
            TestDescriptionSection::getContent);

    assertEquals("foo intro", desc.getIntroductionSection());
    assertEquals("", desc.getRootCauseSection());
    assertEquals("", desc.getHowToFixSection());
    assertEquals("flarp resources", desc.getResourcesSection());
  }

  @Test
  void testFromDescriptionSectionsUnknownKeys() {
    var desc =
        RemoteRuleDescription.fromDescriptionSections(
            List.of(
                new TestDescriptionSection("introduction", "foo intro"),
                new TestDescriptionSection("custom_1", "bar custom 1"),
                new TestDescriptionSection("custom_2", "baz custom 2")),
            TestDescriptionSection::getKey,
            TestDescriptionSection::getContent);

    assertEquals("foo intro", desc.getIntroductionSection());
    assertEquals("", desc.getRootCauseSection());
    assertEquals("", desc.getHowToFixSection());
    assertEquals("", desc.getResourcesSection());
  }
}
