% Mon Dec 05 16:56:38 CET 2016

\version "2.18.1"

\header { }

\layout { }

\paper { }

\score {
<<
  
  <<
    \new ChordNames { \tempo 4 = 105 < c e g >1 | < e g b >1 | < f a c' >1 | < g b d' f' >1 | < c e g >1 | < a c' e' >1 | < d f a >1 | < g b d' >1 | } 
    { \set Voice.midiInstrument = #"electric bass (pick)" \clef bass c,4 \tuplet 3/2 { c,8 c,8 c,8 } \tuplet 3/2 { c,8 r8 g,8 } \tuplet 3/2 { c,8 c,8 c,8 }   | e,4 \tuplet 3/2 { g,8 g,8 r8 } \tuplet 3/2 { e,8 r8 b,8 } \tuplet 3/2 { e,8 r8 r8 }   | f,4 \tuplet 3/2 { c8 r8 c8 } \tuplet 3/2 { f,8 r8 f,8 } \tuplet 3/2 { a,8 r8 c8 }   | g,4 \tuplet 3/2 { d8 r8 g,8 } \tuplet 3/2 { b,8 r8 b,8 } \tuplet 3/2 { d8 r8 f8 }   | c,4 \tuplet 3/2 { e,8 g,8 e,8 } \tuplet 3/2 { g,8 r8 c,8 } \tuplet 3/2 { e,8 g,8 e,8 }   | a,4 \tuplet 3/2 { r8 r8 a,8 } \tuplet 3/2 { r8 r8 r8 } \tuplet 3/2 { a,8 r8 e8 }   | d,4 \tuplet 3/2 { d,8 a,8 d,8 } \tuplet 3/2 { f,8 a,8 r8 } \tuplet 3/2 { f,8 r8 a,8 }   | g,4 \tuplet 3/2 { b,8 r8 g,8 } \tuplet 3/2 { b,8 r8 b,8 } \tuplet 3/2 { g,8 r8 r8 }   | } 
  >> 
>>
\layout { }
\midi {
  \context {
    \Staff
    \remove "Staff_performer"
  }
  \context {
    \Voice
    \consists "Staff_performer"
  }
}
}