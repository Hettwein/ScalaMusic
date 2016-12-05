% Mon Dec 05 16:53:16 CET 2016

\version "2.18.1"

\header { }

\layout { }

\paper { }

\score {
<<
  
  <<
    \new ChordNames { \tempo 4 = 105 < c e g >1 | < e g b >1 | < f a c' >1 | < g b d' f' >1 | < c e g >1 | < a c' e' >1 | < d f a >1 | < g b d' >1 | } 
    { \set Voice.midiInstrument = #"electric bass (pick)" \clef bass c,4 e,8 e,4 c,8 e,8 c,8   | e,4 g,4 g,8 e,4 b,8   | f,4 a,8 f,8 f,8 a,4 c8   | g,4 d4 b,8 g,8 d8 d8   | c,4 c,8 c,8 c,8 g,4 c,8   | a,4 a,8 c8 c4 a,8 a,8   | d,4 d,8 a,4 d,4 d,8   | g,4 b,4 b,8 b,8 b,8 g,8   | } 
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