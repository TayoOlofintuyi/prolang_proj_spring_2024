import SinWaves

c_major_phrase :: Phrase
c_major_phrase = make_phrase [
    make_note nC3 1 1,
    make_note nD3 1 1,
    make_note nE3 1 1,
    make_note nF3 1 1,
    make_note nG3 1 1,
    make_note nA3 1 1,
    make_note nB3 1 1,
    make_note nC4 1 1
    ]

c_up_down :: Phrase
c_up_down = make_phrase [
    make_note nC3 1 1,
    make_note nD3 1 1,
    make_note nE3 1 1,
    make_note nF3 1 1,
    make_note nG3 1 1,
    make_note nA3 1 1,
    make_note nB3 1 1,
    make_note nC4 1 1,
    make_note nB3 1 1,
    make_note nA3 1 1,
    make_note nG3 1 1,
    make_note nF3 1 1,
    make_note nE3 1 1,
    make_note nD3 1 1,
    make_note nC3 1 1
    ]

twinkle_phrase :: Phrase
twinkle_phrase = make_phrase [
        make_note nC4 1 1,
        make_note 0 0 1,
        make_note nC4 1 1,
        make_note nG4 1 1,
        make_note 0 0 1,
        make_note nG4 1 1,
        make_note nA4 1 1,
        make_note 0 0 1,
        make_note nA4 1 1,
        make_note nG4 2 1,
        make_note nF4 1 1,
        make_note 0 0 1,
        make_note nF4 1 1,
        make_note nE4 1 1,
        make_note 0 0 1,
        make_note nE4 1 1,
        make_note nD4 1 1,
        make_note 0 0 1,
        make_note nD4 1 1,
        make_note nC4 2 1
        ]

up_above_the_world :: Phrase
up_above_the_world = make_phrase [
        make_note nG4 1 1,
        make_note 0 0 1,
        make_note nG4 1 1,
        make_note nF4 1 1,
        make_note 0 0 1,
        make_note nF4 1 1,
        make_note nE4 1 1,
        make_note 0 0 1,
        make_note nE4 1 1,
        make_note nD4 2 1
        ]

scale_5_phrase :: Phrase
scale_5_phrase = make_phrase [
        make_note nC3 1 1,
        make_note nD3 1 1,
        make_note nE3 1 1,
        make_note nF3 1 1,
        make_note nG3 1 1,
        make_note nF3 1 1,
        make_note nE3 1 1,
        make_note nD3 1 1,
        make_note nC3 2 1,
        make_note nC3 0 1,
        make_note nC3 2 1,
        make_note nCsharp3 3 1,
        make_note nCsharp3 0 1
        ]



