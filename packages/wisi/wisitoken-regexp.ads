--  Abstract:
--
--  Regular expressions, for WisiToken.Lexer.Regexp. It supports a
--  subset of the syntax of regular expressions copied from familiar
--  Unix style utilities.
--
--  Copied from GNAT System.Regexp, modified to expose the matcher
--  state machine to allow matching substrings.
--
--  Copyright (C) 2015 Stephen Leake
--  Copyright (C) 1998-2010, AdaCore
--
--  This file is part of the WisiToken package.
--
--  The WisiToken package is free software; you can redistribute it
--  and/or modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 3, or
--  (at your option) any later version. The WisiToken package is
--  distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
--  License for more details.
--
--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License. This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.
--
--  You should have received a copy of the
--  GNU General Public License and
--  a copy of the GCC Runtime Library Exception distributed with the WisiToken package;
--  see files GPL.txt and GPL_runtime.txt. If not, see
--  <http://www.gnu.org/licenses/>.

pragma License (Modified_GPL);

with Ada.Finalization;
package WisiToken.Regexp is

   --  The regular expression must first be compiled, using the Compile
   --  function, which creates a finite state matching table, allowing
   --  very fast matching.

   --  The following is the form of a regular expression, expressed in Ada
   --  reference manual style BNF

   --     regexp ::= term

   --     regexp ::= term | term          -- alternation (term or term ...)

   --     term ::= item

   --     term ::= item item ...          -- concatenation (item then item)

   --     item ::= elmt                   -- match elmt
   --     item ::= elmt *                 -- zero or more elmt's
   --     item ::= elmt +                 -- one or more elmt's
   --     item ::= elmt ?                 -- matches elmt or nothing

   --     elmt ::= nchr                   -- matches given character
   --     elmt ::= [nchr nchr ...]        -- matches any character listed
   --     elmt ::= [^ nchr nchr ...]      -- matches any character not listed
   --     elmt ::= [char - char]          -- matches chars in given range
   --     elmt ::= .                      -- matches any single character
   --     elmt ::= ( regexp )             -- parens used for grouping

   --     char ::= any character, including special characters
   --     nchr ::= any character except \()[].*+?^ or \char to match char
   --     ... is used to indication repetition (one or more terms)

   --  See also regexp(1) man page on Unix systems for further details

   type Regexp is private;

   Error_In_Regexp : exception;

   function Compile
     (Pattern        : String;
      Case_Sensitive : Boolean := True)
     return Regexp;
   --  Compile a Pattern. If the syntax of the given
   --  expression is invalid (does not match above grammar), Error_In_Regexp
   --  is raised.
   --
   --  If Pattern is the empty string it will raise Error_In_Regexp
   --
   --  Raises Error_In_Regexp when an error is found in the regular
   --  expression

   procedure Clear (R : in out Regexp);
   --  Clear R internal state, to prepare for a new Match.

   type Match_State is (Matching, Final, Error);

   function Match (R : in out Regexp; S : String; Next : Integer) return Match_State;
   --  Compute match for S (Next), assuming Next is the next character
   --  in S after the previous call to Match. If this is the first
   --  call, Next must be S'First.
   --
   --  S'First, S'Last may change between calls to Match.
   --
   --  The return values mean:
   --
   --  Matching: S (S'First .. Next) matches R, but more characters
   --  are needed to complete the match.
   --
   --  Final: S (S'First .. Next) matches R; more characters may also.
   --
   --  Error: S (S'First .. Next) does not match R.
   --
   --  Raises Constraint_Error if R is not a compiled regular
   --  expression.

   function State (R : in Regexp) return Match_State;
   --  Return the current state of R; if Matching, it is useful to
   --  call Match again.
   --
   --  After Clear (R), State (R) returns Matching.

private
   type Regexp_Value;

   type Regexp_Access is access Regexp_Value;

   type Regexp is new Ada.Finalization.Controlled with record
      R : Regexp_Access := null;
   end record;

   pragma Finalize_Storage_Only (Regexp);

   overriding procedure Finalize (R : in out Regexp);
   --  Free the memory occupied by R

   overriding procedure Adjust (R : in out Regexp);
   --  Deep copy R.R.all

end WisiToken.Regexp;
