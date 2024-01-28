module Atoms exposing
    ( AtomInfo, Atom(..)
    , create
    , get, filter
    , PeriodicTableInfo, periodicTable
    )

{-| This module provides a custom type representing chemical elements and a record type for store informations about them.


# Types

@docs AtomInfo, Atom

# Data

@docs PeriodicTableInfo, periodicTable


# Creation

@docs create


# Query

@docs get, filter



-}


{-| A Record type alias containing some information about all 118 atoms (as of 2024) in the periodic table. The names of atom in US English are used as the record key.

    periodicTable.hydrogen.atomicNumber --> 1

    periodicTable.carbon.atomicNumber --> 6

-}
type alias AtomInfo a =
    { hydrogen : a
    , helium : a
    , lithium : a
    , beryllium : a
    , boron : a
    , carbon : a
    , nitrogen : a
    , oxygen : a
    , fluorine : a
    , neon : a
    , sodium : a
    , magnesium : a
    , aluminum : a
    , silicon : a
    , phosphorus : a
    , sulfur : a
    , chlorine : a
    , argon : a
    , potassium : a
    , calcium : a
    , scandium : a
    , titanium : a
    , vanadium : a
    , chromium : a
    , manganese : a
    , iron : a
    , cobalt : a
    , nickel : a
    , copper : a
    , zinc : a
    , gallium : a
    , germanium : a
    , arsenic : a
    , selenium : a
    , bromine : a
    , krypton : a
    , rubidium : a
    , strontium : a
    , yttrium : a
    , zirconium : a
    , niobium : a
    , molybdenum : a
    , technetium : a
    , ruthenium : a
    , rhodium : a
    , palladium : a
    , silver : a
    , cadmium : a
    , indium : a
    , tin : a
    , antimony : a
    , tellurium : a
    , iodine : a
    , xenon : a
    , cesium : a
    , barium : a
    , lanthanum : a
    , cerium : a
    , praseodymium : a
    , neodymium : a
    , promethium : a
    , samarium : a
    , europium : a
    , gadolinium : a
    , terbium : a
    , dysprosium : a
    , holmium : a
    , erbium : a
    , thulium : a
    , ytterbium : a
    , lutetium : a
    , hafnium : a
    , tantalum : a
    , tungsten : a
    , rhenium : a
    , osmium : a
    , iridium : a
    , platinum : a
    , gold : a
    , mercury : a
    , thallium : a
    , lead : a
    , bismuth : a
    , polonium : a
    , astatine : a
    , radon : a
    , francium : a
    , radium : a
    , actinium : a
    , thorium : a
    , protactinium : a
    , uranium : a
    , neptunium : a
    , plutonium : a
    , americium : a
    , curium : a
    , berkelium : a
    , californium : a
    , einsteinium : a
    , fermium : a
    , mendelevium : a
    , nobelium : a
    , lawrencium : a
    , rutherfordium : a
    , dubnium : a
    , seaborgium : a
    , bohrium : a
    , hassium : a
    , meitnerium : a
    , darmstadtium : a
    , roentgenium : a
    , copernicium : a
    , nihonium : a
    , flerovium : a
    , moscovium : a
    , livermorium : a
    , tennessine : a
    , oganesson : a
    }


{-| A Custom type representing all 118 atoms (as of 2024) in the periodic table.
-}
type Atom
    = H
    | He
    | Li
    | Be
    | B
    | C
    | N
    | O
    | F
    | Ne
    | Na
    | Mg
    | Al
    | Si
    | P
    | S
    | Cl
    | Ar
    | K
    | Ca
    | Sc
    | Ti
    | V
    | Cr
    | Mn
    | Fe
    | Co
    | Ni
    | Cu
    | Zn
    | Ga
    | Ge
    | As
    | Se
    | Br
    | Kr
    | Rb
    | Sr
    | Y
    | Zr
    | Nb
    | Mo
    | Tc
    | Ru
    | Rh
    | Pd
    | Ag
    | Cd
    | In
    | Sn
    | Sb
    | Te
    | I
    | Xe
    | Cs
    | Ba
    | La
    | Ce
    | Pr
    | Nd
    | Pm
    | Sm
    | Eu
    | Gd
    | Tb
    | Dy
    | Ho
    | Er
    | Tm
    | Yb
    | Lu
    | Hf
    | Ta
    | W
    | Re
    | Os
    | Ir
    | Pt
    | Au
    | Hg
    | Tl
    | Pb
    | Bi
    | Po
    | At
    | Rn
    | Fr
    | Ra
    | Ac
    | Th
    | Pa
    | U
    | Np
    | Pu
    | Am
    | Cm
    | Bk
    | Cf
    | Es
    | Fm
    | Md
    | No
    | Lr
    | Rf
    | Db
    | Sg
    | Bh
    | Hs
    | Mt
    | Ds
    | Rg
    | Cn
    | Nh
    | Fl
    | Mc
    | Lv
    | Ts
    | Og


{-| Returns value of an atom stored in the second argument. You also can directly access record like `atoms.hydrogen`, which has a better performance in return for a bit more keyboad typing.

    calcium : PeriodicTableInfo
    calcium = get Ca periodicTable

    calcium.period --> 4

    calcium.group --> 2

-}
get : Atom -> AtomInfo a -> a
get a o =
    case a of
        H ->
            o.hydrogen

        He ->
            o.helium

        Li ->
            o.lithium

        Be ->
            o.beryllium

        B ->
            o.boron

        C ->
            o.carbon

        N ->
            o.nitrogen

        O ->
            o.oxygen

        F ->
            o.fluorine

        Ne ->
            o.neon

        Na ->
            o.sodium

        Mg ->
            o.magnesium

        Al ->
            o.aluminum

        Si ->
            o.silicon

        P ->
            o.phosphorus

        S ->
            o.sulfur

        Cl ->
            o.chlorine

        Ar ->
            o.argon

        K ->
            o.potassium

        Ca ->
            o.calcium

        Sc ->
            o.scandium

        Ti ->
            o.titanium

        V ->
            o.vanadium

        Cr ->
            o.chromium

        Mn ->
            o.manganese

        Fe ->
            o.iron

        Co ->
            o.cobalt

        Ni ->
            o.nickel

        Cu ->
            o.copper

        Zn ->
            o.zinc

        Ga ->
            o.gallium

        Ge ->
            o.germanium

        As ->
            o.arsenic

        Se ->
            o.selenium

        Br ->
            o.bromine

        Kr ->
            o.krypton

        Rb ->
            o.rubidium

        Sr ->
            o.strontium

        Y ->
            o.yttrium

        Zr ->
            o.zirconium

        Nb ->
            o.niobium

        Mo ->
            o.molybdenum

        Tc ->
            o.technetium

        Ru ->
            o.ruthenium

        Rh ->
            o.rhodium

        Pd ->
            o.palladium

        Ag ->
            o.silver

        Cd ->
            o.cadmium

        In ->
            o.indium

        Sn ->
            o.tin

        Sb ->
            o.antimony

        Te ->
            o.tellurium

        I ->
            o.iodine

        Xe ->
            o.xenon

        Cs ->
            o.cesium

        Ba ->
            o.barium

        La ->
            o.lanthanum

        Ce ->
            o.cerium

        Pr ->
            o.praseodymium

        Nd ->
            o.neodymium

        Pm ->
            o.promethium

        Sm ->
            o.samarium

        Eu ->
            o.europium

        Gd ->
            o.gadolinium

        Tb ->
            o.terbium

        Dy ->
            o.dysprosium

        Ho ->
            o.holmium

        Er ->
            o.erbium

        Tm ->
            o.thulium

        Yb ->
            o.ytterbium

        Lu ->
            o.lutetium

        Hf ->
            o.hafnium

        Ta ->
            o.tantalum

        W ->
            o.tungsten

        Re ->
            o.rhenium

        Os ->
            o.osmium

        Ir ->
            o.iridium

        Pt ->
            o.platinum

        Au ->
            o.gold

        Hg ->
            o.mercury

        Tl ->
            o.thallium

        Pb ->
            o.lead

        Bi ->
            o.bismuth

        Po ->
            o.polonium

        At ->
            o.astatine

        Rn ->
            o.radon

        Fr ->
            o.francium

        Ra ->
            o.radium

        Ac ->
            o.actinium

        Th ->
            o.thorium

        Pa ->
            o.protactinium

        U ->
            o.uranium

        Np ->
            o.neptunium

        Pu ->
            o.plutonium

        Am ->
            o.americium

        Cm ->
            o.curium

        Bk ->
            o.berkelium

        Cf ->
            o.californium

        Es ->
            o.einsteinium

        Fm ->
            o.fermium

        Md ->
            o.mendelevium

        No ->
            o.nobelium

        Lr ->
            o.lawrencium

        Rf ->
            o.rutherfordium

        Db ->
            o.dubnium

        Sg ->
            o.seaborgium

        Bh ->
            o.bohrium

        Hs ->
            o.hassium

        Mt ->
            o.meitnerium

        Ds ->
            o.darmstadtium

        Rg ->
            o.roentgenium

        Cn ->
            o.copernicium

        Nh ->
            o.nihonium

        Fl ->
            o.flerovium

        Mc ->
            o.moscovium

        Lv ->
            o.livermorium

        Ts ->
            o.tennessine

        Og ->
            o.oganesson


{-| A record type containing minimal informations for each atoms in the periodic table.
-}
type alias PeriodicTableInfo =
    { atomicNumber : Int, period : Int, group : Int, symbol : String, name : String }


{-| An `Atoms`record containing `PeriodicTableInfo` of the all 118 atoms.

    periodicTable.sodium -->
        { atomicNumber = 11
        , period = 3
        , group = 1
        , symbol = "Na"
        , name = "sodium"
        }

-}
periodicTable : AtomInfo PeriodicTableInfo
periodicTable =
    AtomInfo
        (PeriodicTableInfo 1 1 1 "H" "hydrogen")
        (PeriodicTableInfo 2 1 2 "He" "helium")
        (PeriodicTableInfo 3 2 1 "Li" "lithium")
        (PeriodicTableInfo 4 2 2 "Be" "beryllium")
        (PeriodicTableInfo 5 2 3 "B" "boron")
        (PeriodicTableInfo 6 2 4 "C" "carbon")
        (PeriodicTableInfo 7 2 5 "N" "nitrogen")
        (PeriodicTableInfo 8 2 6 "O" "oxygen")
        (PeriodicTableInfo 9 2 7 "F" "fluorine")
        (PeriodicTableInfo 10 2 8 "Ne" "neon")
        (PeriodicTableInfo 11 3 1 "Na" "sodium")
        (PeriodicTableInfo 12 3 2 "Mg" "magnesium")
        (PeriodicTableInfo 13 3 3 "Al" "aluminum")
        (PeriodicTableInfo 14 3 4 "Si" "silicon")
        (PeriodicTableInfo 15 3 5 "P" "phosphorus")
        (PeriodicTableInfo 16 3 6 "S" "sulfur")
        (PeriodicTableInfo 17 3 7 "Cl" "chlorine")
        (PeriodicTableInfo 18 3 8 "Ar" "argon")
        (PeriodicTableInfo 19 4 1 "K" "potassium")
        (PeriodicTableInfo 20 4 2 "Ca" "calcium")
        (PeriodicTableInfo 21 4 3 "Sc" "scandium")
        (PeriodicTableInfo 22 4 4 "Ti" "titanium")
        (PeriodicTableInfo 23 4 5 "V" "vanadium")
        (PeriodicTableInfo 24 4 6 "Cr" "chromium")
        (PeriodicTableInfo 25 4 7 "Mn" "manganese")
        (PeriodicTableInfo 26 4 8 "Fe" "iron")
        (PeriodicTableInfo 27 4 9 "Co" "cobalt")
        (PeriodicTableInfo 28 4 10 "Ni" "nickel")
        (PeriodicTableInfo 29 4 11 "Cu" "copper")
        (PeriodicTableInfo 30 4 12 "Zn" "zinc")
        (PeriodicTableInfo 31 4 13 "Ga" "gallium")
        (PeriodicTableInfo 32 4 14 "Ge" "germanium")
        (PeriodicTableInfo 33 4 15 "As" "arsenic")
        (PeriodicTableInfo 34 4 16 "Se" "selenium")
        (PeriodicTableInfo 35 4 17 "Br" "bromine")
        (PeriodicTableInfo 36 4 18 "Kr" "krypton")
        (PeriodicTableInfo 37 5 1 "Rb" "rubidium")
        (PeriodicTableInfo 38 5 2 "Sr" "strontium")
        (PeriodicTableInfo 39 5 3 "Y" "yttrium")
        (PeriodicTableInfo 40 5 4 "Zr" "zirconium")
        (PeriodicTableInfo 41 5 5 "Nb" "niobium")
        (PeriodicTableInfo 42 5 6 "Mo" "molybdenum")
        (PeriodicTableInfo 43 5 7 "Tc" "technetium")
        (PeriodicTableInfo 44 5 8 "Ru" "ruthenium")
        (PeriodicTableInfo 45 5 9 "Rh" "rhodium")
        (PeriodicTableInfo 46 5 10 "Pd" "palladium")
        (PeriodicTableInfo 47 5 11 "Ag" "silver")
        (PeriodicTableInfo 48 5 12 "Cd" "cadmium")
        (PeriodicTableInfo 49 5 13 "In" "indium")
        (PeriodicTableInfo 50 5 14 "Sn" "tin")
        (PeriodicTableInfo 51 5 15 "Sb" "antimony")
        (PeriodicTableInfo 52 5 16 "Te" "tellurium")
        (PeriodicTableInfo 53 5 17 "I" "iodine")
        (PeriodicTableInfo 54 5 18 "Xe" "xenon")
        (PeriodicTableInfo 55 6 1 "Cs" "cesium")
        (PeriodicTableInfo 56 6 2 "Ba" "barium")
        (PeriodicTableInfo 57 6 3 "La" "lanthanum")
        (PeriodicTableInfo 58 6 3 "Ce" "cerium")
        (PeriodicTableInfo 59 6 3 "Pr" "praseodymium")
        (PeriodicTableInfo 60 6 3 "Nd" "neodymium")
        (PeriodicTableInfo 61 6 3 "Pm" "promethium")
        (PeriodicTableInfo 62 6 3 "Sm" "samarium")
        (PeriodicTableInfo 63 6 3 "Eu" "europium")
        (PeriodicTableInfo 64 6 3 "Gd" "gadolinium")
        (PeriodicTableInfo 65 6 3 "Tb" "terbium")
        (PeriodicTableInfo 66 6 3 "Dy" "dysprosium")
        (PeriodicTableInfo 67 6 3 "Ho" "holmium")
        (PeriodicTableInfo 68 6 3 "Er" "erbium")
        (PeriodicTableInfo 69 6 3 "Tm" "thulium")
        (PeriodicTableInfo 70 6 3 "Yb" "ytterbium")
        (PeriodicTableInfo 71 6 3 "Lu" "lutetium")
        (PeriodicTableInfo 72 6 4 "Hf" "hafnium")
        (PeriodicTableInfo 73 6 5 "Ta" "tantalum")
        (PeriodicTableInfo 74 6 6 "W" "tungsten")
        (PeriodicTableInfo 75 6 7 "Re" "rhenium")
        (PeriodicTableInfo 76 6 8 "Os" "osmium")
        (PeriodicTableInfo 77 6 9 "Ir" "iridium")
        (PeriodicTableInfo 78 6 10 "Pt" "platinum")
        (PeriodicTableInfo 79 6 11 "Au" "gold")
        (PeriodicTableInfo 80 6 12 "Hg" "mercury")
        (PeriodicTableInfo 81 6 13 "Tl" "thallium")
        (PeriodicTableInfo 82 6 14 "Pb" "lead")
        (PeriodicTableInfo 83 6 15 "Bi" "bismuth")
        (PeriodicTableInfo 84 6 16 "Po" "polonium")
        (PeriodicTableInfo 85 6 17 "At" "astatine")
        (PeriodicTableInfo 86 6 18 "Rn" "radon")
        (PeriodicTableInfo 87 7 1 "Fr" "francium")
        (PeriodicTableInfo 88 7 2 "Ra" "radium")
        (PeriodicTableInfo 89 7 3 "Ac" "actinium")
        (PeriodicTableInfo 90 7 3 "Th" "thorium")
        (PeriodicTableInfo 91 7 3 "Pa" "protactinium")
        (PeriodicTableInfo 92 7 3 "U" "uranium")
        (PeriodicTableInfo 93 7 3 "Np" "neptunium")
        (PeriodicTableInfo 94 7 3 "Pu" "plutonium")
        (PeriodicTableInfo 95 7 3 "Am" "americium")
        (PeriodicTableInfo 96 7 3 "Cm" "curium")
        (PeriodicTableInfo 97 7 3 "Bk" "berkelium")
        (PeriodicTableInfo 98 7 3 "Cf" "californium")
        (PeriodicTableInfo 99 7 3 "Es" "einsteinium")
        (PeriodicTableInfo 100 7 3 "Fm" "fermium")
        (PeriodicTableInfo 101 7 3 "Md" "mendelevium")
        (PeriodicTableInfo 102 7 3 "No" "nobelium")
        (PeriodicTableInfo 103 7 3 "Lr" "lawrencium")
        (PeriodicTableInfo 104 7 4 "Rf" "rutherfordium")
        (PeriodicTableInfo 105 7 5 "Db" "dubnium")
        (PeriodicTableInfo 106 7 6 "Sg" "seaborgium")
        (PeriodicTableInfo 107 7 7 "Bh" "bohrium")
        (PeriodicTableInfo 108 7 8 "Hs" "hassium")
        (PeriodicTableInfo 109 7 9 "Mt" "meitnerium")
        (PeriodicTableInfo 110 7 10 "Ds" "darmstadtium")
        (PeriodicTableInfo 111 7 11 "Rg" "roentgenium")
        (PeriodicTableInfo 112 7 12 "Cn" "copernicium")
        (PeriodicTableInfo 113 7 13 "Nh" "nihonium")
        (PeriodicTableInfo 114 7 14 "Fl" "flerovium")
        (PeriodicTableInfo 115 7 15 "Mc" "moscovium")
        (PeriodicTableInfo 116 7 16 "Lv" "livermorium")
        (PeriodicTableInfo 117 7 17 "Ts" "tennessine")
        (PeriodicTableInfo 118 7 18 "Og" "oganesson")


{-| Returns new `AtomInfo` having specified informations.

    type Metality
        = Metal
        | NonMetal

    metality : AtomInfo Metality
    metality =
        create
            (\atom ->
                let
                    { period, group } =
                        get atom periodicTable
                in
                if
                    (period == 1)
                        || (period <= 6)
                                && (group >= 10 + period)
                then
                    NonMetal
                else
                    Metal
            )

    get Xe metality --> NonMetal

    get U metality --> Metal
    
-}
create : (Atom -> a) -> AtomInfo a
create map =
    { hydrogen = map H
    , helium = map He
    , lithium = map Li
    , beryllium = map Be
    , boron = map B
    , carbon = map C
    , nitrogen = map N
    , oxygen = map O
    , fluorine = map F
    , neon = map Ne
    , sodium = map Na
    , magnesium = map Mg
    , aluminum = map Al
    , silicon = map Si
    , phosphorus = map P
    , sulfur = map S
    , chlorine = map Cl
    , argon = map Ar
    , potassium = map K
    , calcium = map Ca
    , scandium = map Sc
    , titanium = map Ti
    , vanadium = map V
    , chromium = map Cr
    , manganese = map Mn
    , iron = map Fe
    , cobalt = map Co
    , nickel = map Ni
    , copper = map Cu
    , zinc = map Zn
    , gallium = map Ga
    , germanium = map Ge
    , arsenic = map As
    , selenium = map Se
    , bromine = map Br
    , krypton = map Kr
    , rubidium = map Rb
    , strontium = map Sr
    , yttrium = map Y
    , zirconium = map Zr
    , niobium = map Nb
    , molybdenum = map Mo
    , technetium = map Tc
    , ruthenium = map Ru
    , rhodium = map Rh
    , palladium = map Pd
    , silver = map Ag
    , cadmium = map Cd
    , indium = map In
    , tin = map Sn
    , antimony = map Sb
    , tellurium = map Te
    , iodine = map I
    , xenon = map Xe
    , cesium = map Cs
    , barium = map Ba
    , lanthanum = map La
    , cerium = map Ce
    , praseodymium = map Pr
    , neodymium = map Nd
    , promethium = map Pm
    , samarium = map Sm
    , europium = map Eu
    , gadolinium = map Gd
    , terbium = map Tb
    , dysprosium = map Dy
    , holmium = map Ho
    , erbium = map Er
    , thulium = map Tm
    , ytterbium = map Yb
    , lutetium = map Lu
    , hafnium = map Hf
    , tantalum = map Ta
    , tungsten = map W
    , rhenium = map Re
    , osmium = map Os
    , iridium = map Ir
    , platinum = map Pt
    , gold = map Au
    , mercury = map Hg
    , thallium = map Tl
    , lead = map Pb
    , bismuth = map Bi
    , polonium = map Po
    , astatine = map At
    , radon = map Rn
    , francium = map Fr
    , radium = map Ra
    , actinium = map Ac
    , thorium = map Th
    , protactinium = map Pa
    , uranium = map U
    , neptunium = map Np
    , plutonium = map Pu
    , americium = map Am
    , curium = map Cm
    , berkelium = map Bk
    , californium = map Cf
    , einsteinium = map Es
    , fermium = map Fm
    , mendelevium = map Md
    , nobelium = map No
    , lawrencium = map Lr
    , rutherfordium = map Rf
    , dubnium = map Db
    , seaborgium = map Sg
    , bohrium = map Bh
    , hassium = map Hs
    , meitnerium = map Mt
    , darmstadtium = map Ds
    , roentgenium = map Rg
    , copernicium = map Cn
    , nihonium = map Nh
    , flerovium = map Fl
    , moscovium = map Mc
    , livermorium = map Lv
    , tennessine = map Ts
    , oganesson = map Og
    }


{-| Returns list of atoms that passed a test ordered with ascending atomic number.

    first10Atoms : List Atom
    first10Atoms =
        filter (\a -> a.atomicNumber <= 10) periodicTable -->
            [ H
            , He
            , Li
            , Be
            , B
            , C
            , N
            , O
            , F
            , Ne
            ]

-}
filter : (a -> Bool) -> AtomInfo a -> List Atom
filter f atoms =
    filterHelp f atoms reversedAtomList []


filterHelp : (a -> Bool) -> AtomInfo a -> List ( Atom, AtomInfo a -> a ) -> List Atom -> List Atom
filterHelp f atoms rest sofar =
    case rest of
        ( atom, getter ) :: tail ->
            if f (getter atoms) then
                filterHelp f atoms tail (atom :: sofar)

            else
                filterHelp f atoms tail sofar

        [] ->
            sofar


reversedAtomList : List ( Atom, AtomInfo a -> a )
reversedAtomList =
    [ ( Og, .oganesson )
    , ( Ts, .tennessine )
    , ( Lv, .livermorium )
    , ( Mc, .moscovium )
    , ( Fl, .flerovium )
    , ( Nh, .nihonium )
    , ( Cn, .copernicium )
    , ( Rg, .roentgenium )
    , ( Ds, .darmstadtium )
    , ( Mt, .meitnerium )
    , ( Hs, .hassium )
    , ( Bh, .bohrium )
    , ( Sg, .seaborgium )
    , ( Db, .dubnium )
    , ( Rf, .rutherfordium )
    , ( Lr, .lawrencium )
    , ( No, .nobelium )
    , ( Md, .mendelevium )
    , ( Fm, .fermium )
    , ( Es, .einsteinium )
    , ( Cf, .californium )
    , ( Bk, .berkelium )
    , ( Cm, .curium )
    , ( Am, .americium )
    , ( Pu, .plutonium )
    , ( Np, .neptunium )
    , ( U, .uranium )
    , ( Pa, .protactinium )
    , ( Th, .thorium )
    , ( Ac, .actinium )
    , ( Ra, .radium )
    , ( Fr, .francium )
    , ( Rn, .radon )
    , ( At, .astatine )
    , ( Po, .polonium )
    , ( Bi, .bismuth )
    , ( Pb, .lead )
    , ( Tl, .thallium )
    , ( Hg, .mercury )
    , ( Au, .gold )
    , ( Pt, .platinum )
    , ( Ir, .iridium )
    , ( Os, .osmium )
    , ( Re, .rhenium )
    , ( W, .tungsten )
    , ( Ta, .tantalum )
    , ( Hf, .hafnium )
    , ( Lu, .lutetium )
    , ( Yb, .ytterbium )
    , ( Tm, .thulium )
    , ( Er, .erbium )
    , ( Ho, .holmium )
    , ( Dy, .dysprosium )
    , ( Tb, .terbium )
    , ( Gd, .gadolinium )
    , ( Eu, .europium )
    , ( Sm, .samarium )
    , ( Pm, .promethium )
    , ( Nd, .neodymium )
    , ( Pr, .praseodymium )
    , ( Ce, .cerium )
    , ( La, .lanthanum )
    , ( Ba, .barium )
    , ( Cs, .cesium )
    , ( Xe, .xenon )
    , ( I, .iodine )
    , ( Te, .tellurium )
    , ( Sb, .antimony )
    , ( Sn, .tin )
    , ( In, .indium )
    , ( Cd, .cadmium )
    , ( Ag, .silver )
    , ( Pd, .palladium )
    , ( Rh, .rhodium )
    , ( Ru, .ruthenium )
    , ( Tc, .technetium )
    , ( Mo, .molybdenum )
    , ( Nb, .niobium )
    , ( Zr, .zirconium )
    , ( Y, .yttrium )
    , ( Sr, .strontium )
    , ( Rb, .rubidium )
    , ( Kr, .krypton )
    , ( Br, .bromine )
    , ( Se, .selenium )
    , ( As, .arsenic )
    , ( Ge, .germanium )
    , ( Ga, .gallium )
    , ( Zn, .zinc )
    , ( Cu, .copper )
    , ( Ni, .nickel )
    , ( Co, .cobalt )
    , ( Fe, .iron )
    , ( Mn, .manganese )
    , ( Cr, .chromium )
    , ( V, .vanadium )
    , ( Ti, .titanium )
    , ( Sc, .scandium )
    , ( Ca, .calcium )
    , ( K, .potassium )
    , ( Ar, .argon )
    , ( Cl, .chlorine )
    , ( S, .sulfur )
    , ( P, .phosphorus )
    , ( Si, .silicon )
    , ( Al, .aluminum )
    , ( Mg, .magnesium )
    , ( Na, .sodium )
    , ( Ne, .neon )
    , ( F, .fluorine )
    , ( O, .oxygen )
    , ( N, .nitrogen )
    , ( C, .carbon )
    , ( B, .boron )
    , ( Be, .beryllium )
    , ( Li, .lithium )
    , ( He, .helium )
    , ( H, .hydrogen )
    ]
