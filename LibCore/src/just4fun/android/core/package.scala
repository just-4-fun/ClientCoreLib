package just4fun.android

import android.util.Log
import scala.language.implicitConversions

package object core {


	/* LOGGING */



}

/* Scala Doc Quick ref
Inline elements
Italic: ''text'' becomes text.
Bold: '''text''' becomes text.
Underline: __text__ becomes text.
Monospace: `text` becomes text.
Superscript: ^text^ becomes text.
Subscript: ,,text,, becomes text.
Entity links: [[scala.collection.Seq]] becomes a link to the corresponding entity like Seq. The entity must be linked using fully-qualified names (scala.collection.Seq instead of Seq); this requirement may be relaxed in a future release.
External links: [[http://scala-lang.org Scala web site]] becomes Scala web site. The URL part must start with a scheme name (like http:) and must not contain white space. The name part (Scala web site) is optional.
Block elements
Paragraphs: a blank line starts a new paragraph. Note that a "blank line" may contain a left margin * delimiter, as described above.
Code blocks: enclose between {{{ and }}}. See rule above for determining left margin.
Headings: =heading= defines a heading. Lower-level heading are obtained by using more = signs, like ===sub-heading===. A heading must be define on its own line.
List block: A list block is a sequence of list items of the same style and level, uninterrupted by other block elements.
Unordered list item: $ - item becomes a list item ($ signifies the left margin of a line (it doesn't appear in the code), note the space between the left margin and the dash). More leading space characters are allowed between the left margin and the dash: list items with the same number of leading spaces are at the same level, more space creates sub-levels.
Ordered list item: uses similar rules to the unordered item, but replace dash by one of 1., I., i., A. or a. for various numbering styles.

  */