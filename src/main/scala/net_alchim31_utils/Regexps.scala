/**
 * Copyright (C) 2010 Alchim31 http://alchim31.net/
 *
 * http://github.com/davidB/vscaladoc2_genjson
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net_alchim31_utils

object Regexps {
  import java.util.ArrayDeque
  import java.util.regex.{Pattern, PatternSyntaxException}

  /**
   * Return the compiled Java regex Pattern equivalent to the specified GLOB expression.
   *
   * @param glob GLOB expression to be compiled
   * @return equivalent compiled Java regex Pattern
   */
  def globToRegexPattern(glob : String) : Pattern = {
    /* Stack to keep track of the parser mode: */
    /* "--" : Base mode (first on the stack)   */
    /* "[]" : Square brackets mode "[...]"     */
    /* "{}" : Curly braces mode "{...}"        */
    val parserMode = new ArrayDeque[String]()
    parserMode.push("--"); // base mode

    val globLength = glob.length()
    var index = 0 // index in glob

    // equivalent REGEX expression to be compiled
    val t = new StringBuilder()

    while (index < globLength) {
      var c = glob.charAt(index)
      index += 1

      if (c == '\\') {
        // (1) ESCAPE SEQUENCE *
        if (index == globLength) {
          // no characters left, so treat '\' as literal char
          t.append(Pattern.quote("\\"));
        } else {
          // read next character
          c = glob.charAt(index)
          val s = c + ""
          if (("--".equals(parserMode.peek()) && "\\[]{}?*".contains(s)) ||
              ("[]".equals(parserMode.peek()) && "\\[]{}?*!-".contains(s)) ||
              ("{}".equals(parserMode.peek()) && "\\[]{}?*,".contains(s))) {
            // escape the construct char
            index += 1
            t.append(Pattern.quote(s))
          } else {
            /* treat '\' as literal char */
            t.append(Pattern.quote("\\"))
          }
        }
      } else if (c == '*') {
        // (2) GLOB PATTERN '*'
        // create non-capturing group to match zero or more characters
        t.append(".*")
      } else if (c == '?') {
        // (3) GLOB PATTERN '?'
        // create non-capturing group to match exactly one character
        t.append('.')
      } else if (c == '[') {
        // (4) GLOB PATTERN "[...]"
        // opening square bracket '['
        // create non-capturing group to match exactly one character
        // inside the sequence
        t.append('[')
        parserMode.push("[]")
        // check for negation character '!' immediately after the opening bracket '['
        if ((index < globLength) && (glob.charAt(index) == '!')) {
          index += 1;
          t.append('^');
        }
      } else if ((c == ']') && "[]".equals(parserMode.peek())) {
        // closing square bracket ']'
        t.append(']');
        parserMode.pop();
      } else if ((c == '-') && "[]".equals(parserMode.peek())) {
        // character range '-' in "[...]"
        t.append('-')
      } else if (c == '{') {
        // (5) GLOB PATTERN "{...}"
        // opening curly brace '{'
        // create non-capturing group to match one of the
        // strings inside the sequence
        t.append("(?:(?:")
        parserMode.push("{}")
      } else if ((c == '}') && "{}".equals(parserMode.peek())) {
        // closing curly brace '}'
        t.append("))")
        parserMode.pop()
      } else if ((c == ',') && "{}".equals(parserMode.peek())) {
        // comma between strings in "{...}"
        t.append(")|(?:")
      } else {
        // (6) LITERAL CHARACTER
        // convert literal character to a regex string
        t.append(Pattern.quote(c + ""))
      }
    }
    // done parsing all chars of the source pattern string

    // check for mismatched [...] or {...}
    if ("[]".equals(parserMode.peek())) {
      throw new PatternSyntaxException("Cannot find matching closing square bracket ] in GLOB expression", glob, -1);
    }

    if ("{}".equals(parserMode.peek())) {
      throw new PatternSyntaxException("Cannot find matching closing curly brace } in GLOB expression", glob, -1);
    }
    Pattern.compile(t.toString())
  }
}
