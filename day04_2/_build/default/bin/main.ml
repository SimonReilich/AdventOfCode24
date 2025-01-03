let check_up_left str x y = if String.get (List.nth str (y - 1)) (x - 1) = 'M' &&  String.get (List.nth str (y + 1)) (x + 1) = 'S' then 1 else 0
let check_down_left str x y = if String.get (List.nth str (y + 1)) (x - 1) = 'M' && String.get (List.nth str (y - 1)) (x + 1) = 'S' then 1 else 0
let check_up_right str x y = if String.get (List.nth str (y - 1)) (x + 1) = 'M' && String.get (List.nth str (y + 1)) (x - 1) = 'S' then 1 else 0
let check_down_right str x y = if String.get (List.nth str (y + 1)) (x + 1) = 'M' && String.get (List.nth str (y - 1)) (x - 1) = 'S' then 1 else 0

let check_pos str x y = if x < 1 || y < 1 || x >= (String.length (List.nth str 0)) - 1 || y >= List.length str - 1 then 0 else if String.get (List.nth str y) x != 'A' then 0
else if check_up_left str x y + check_down_left str x y + check_up_right str x y + check_down_right str x y >= 2 then 1 else 0

let count_xmas str =
  let rec helper x y acc = if x >= String.length (List.nth str 0) then helper 0 (y + 1) acc else if y >= List.length str then acc else
    helper (x + 1) y (acc + (check_pos str x y))
  in helper 0 0 0

let () = let str = "XASXMAXXMSXXSMMSXMMSMXSMXMSSMSSSMMSMAMXMXSMMMMAXAMXSASXSSMMSSMXAMXMSAMXMMXAXXXSAMXXXXXMMXSXMXXSMASAMXMXAXXMASAMXXXMAMMMSXSXMXMMMSAASXSMSSMMS
XASAMSSMAMMMAMAAAMASMAMAAXMASAAASAAMAMAMAAAAASMSSSSMAXAAAXASASMMMMMMXMASXMAMXXXXSMSMSXSAAXAMXMASMXMMSMSAXSXMMSXSMMAAMXAMAAMMAAAAXMXSAAAAXAAM
MXSXMAAMXAAAXMMSSMAXMASXSSSSMMSMMSXSASMSMSSMMMAAAXXXXMMMMMXXAMMAAAASMMAAAXMASMXXXAAASAMMXSASASMMMAMXSASASASMAXXMAMSSSMSSMMAXSMMSXMMMMMMMXAMX
SAMMMSSMMXXSXXXAAMSMSASXAAXMXXAAXXXMAXAAAXXMASMMMMMMXMXXXXSMXMSSMSASASMMXMMXMAAMASMSMSMAASXSAAAASAXAMAMXAAXMASMSSMXAMMMAMXSAMAMAASXMMSSMSAMX
MASAAAAMSMMMMMMSMMMAMAXMMMMMXMSSMMXMSMMMMMAMXASAXAASXMMASAAXAXAXMXMXMXAASXMAMSXSAMMXMMMMMSAMXMSMSMSMMAMSMXMMASAMXAMMSASAMMXASAMSXMAMXAAXSAMA
SSMXMSMAAMAAXXAMXASMMSMAMXSXSAMXXMAAAAXSXSAMXMASMSXMASXAMSAMXMMMSASMXMXMMAMSMXAMASXMXAASXMMMMXMMXAAXXAMXXAAMAMAMXXXASXSASXMMSAMMASXMMMMMMMMS
XXXXMAMMMSSSMXSXSASXAXSXMASAMXSMXMASMSMSXMASXXMMXMXSAMMXMAXAMAXXSXSAASMASXMXAMXMMMAASMXXAMXMMAXMXMSMSAMSSMSMMSSSSMMMSXMXMASXSMMXAXAXSSSMXAAX
MMXSSXSMXAMAMAAAMSXMXMMXMASXMMSAMXXAAMXSASAMMXXSXMXMASXSSSMXSSMXXMXMMSAXMASMMMSMASAMXMASXSAMSSMSMAAAMAMAXAMAMAMXAXAXSAMXMXAXMMSMMSXMAAAMSAMX
AXMXMASMMASAMXSXMMXSAMAAMXMMMAXMMSMMXMASAMXSXMXMAAMAMSAMAAAMSAMMSXMXSXMXAXMASAMXAXMASXMAMMAMAAASMMMSSSMSSMXSAMXSMSMXXAMXSSMASAMAMMASMMMASMMM
XMAMSXSASASASMXXMAMMAASXSAAAMMXAAXXAMMMMSMSMXMAXMXMAMXAMSMMMSAMASAMXSAMMMMXMMAMMSXXAMAXXAMSMMSMMAXXXMAAAAMAMAXXAMXXSMSAXAAXAMMMAMSXMAXXMSASX
MSSMXASAMXSMMMAXASMSMMAXMAMSASXMAXMMSAXSASAAASMXMASMSSMMMAAXSAMMSAMAMAMMSAMSSSMSAMMSSSMSMXMAXXASXMMMMMMMSMASASXSXMASAXXMSSMMXMXMMMMSAMMXSAMX
MAMMMMMMMMMASAMMMXAAXXMASAAXASMMSXMAXMXSMSMSMSXAMMXXAMXASXMXXAMASAMSSXAAMAMAAMXMASAAAAAAXSSSMMAMASASXXXXMMXSASAXXAAMAMAXXMXSASASAMASAMXAMAMX
MASXAXAMXXSAMMAAXMSMMXMAMXXMMMAAMMMSXMMSXMXAAXXMSAMMSSMMSMMMMAMASAMAXMASXMMMSXAXXMMMSMMMAXAAXXSSMSASMMSAMXXMAMAMXSSMSMSMAMAXXSXSASXMSSMXSAMM
SXMXMSSSMMMASMSMSMAAMASXMMXSASMMXXAMXXASASMMXMAMAAXAMAMAXAASXMMXMMSMXXAXMSXMAXXSSSXAXAAXMMMMMMXAXMMMXAAMAMMMXMAMAMXAMAXAAMASAMASAMXMAMMXSASA
MMMXMXXAXAMAMMAAAMSMMAXAASASAXXXSMSSSMAMXMAMXXASXSMMSMMXSMMXAXSXMAXAMMMXXMAMSAMXAAMSSSMMXAAAAXSAMXXAASMXAASMASXMSSMAMAMMXSAAAMAMAMSMAXSAXAMM
SASASMXMMSXSXSMSMMXSMXXXMMASAXSXMAXAAMMMSSMMMXASXMXMAAXMXMMMXMSAMXSXSASMASAMMSSMMMMAAAXMSSSSMXXAMXMAXXXMSMSAAMAMAMAMMMMXXMMSSMMXAMXXAXMASXSM
MASMMAMSAMMMMAMMXSAMXSMSAMXMMMAAMAMXMAMAMAAAAMSMXSASMXMAAAXSAMXAMAAMSAMSAMXSAMMASAMXSSMMMAMMXXMSMMXMSSMMXAMMXSAMMSAAAAMXAAMAXASXSMSMMSMAMAAX
MMMMMSMMAMXAAMMAAMMXAMMAAAXAXSSXMAMXMAMASMMMXMAAXSASAMSSMSASASXMMMSXMAMXXMMMMXMAMAXAMXAXXSAAASAAAXXXAAMAMAMAAXAMXSASMXSASXMSSMMAMASAMSMMMSXM
SAAMAMAMAMSSSXMMXSSMSSSSSMSSXAAMSXSXSASASAMAMSMSMMMMXAAXAXMSAMASXSMMXSMSMSXSSSMMXMMXSSSMAXMXSXMMAMXMSXMASMMMXSAMAXAXMASMMMMASAMXMAMAMXAXAMXA
SSSMASXMMXMAMXMMAMXAAAAMMAAXAASMSXAAXAMASMMAXAMAAMSAMMMMSMMMXSAMXMAAMXAMAAXMAAXAAMMXMAMMAMMXXAXMAXSAAXSXSXXAASAMAMSMMMXXMXMMSMMSMXSXMSXMXMMS
XAMMAMAMXMMSMMSMAXMMMSMMSMMXMAMAXXMSMSMMMXXMMSSSSMMXMAAAMAXXMMMSXMMMSMAMSMMMSMMSXXASMAMMASMMMAMSSMASMXSAXXMMMXXXMXMXXAMMSASMMMAXMXMXAMXSAMXA
MMXMXSAMMXAXAXMSMSXXAAAAAMSMMXMAMXXAAMASMSXSAAXXAAMAMSSSSSMSXAAXMSMXAMXMAMAMXAMASXXMMMSMMMAMMSMMAMMAXAMAMSXSSMSSMMMASAXASMXAAMXSXAMXMAMXASMS
AXMSMSXXAMXSMMMMXAMMMSMMMSAASAMMMSSMSSXMAAAMXSMXSSMAMMAMMAASXMMSAAXSXSSSSSSMMAMASXAXXASXSSMXAMAXAMXAMMMSMMASXAAXAASAMXMMXXSSMMSMMXSAMSMSMMMA
SMAAAMASXMMSMMXMMMMXAAXASMMMMASXAAAXAXAMSMMMAAAXXAMXXMAMSMMMAXSMMMMMSAAAAAXAXSMMSMMMSAMAXXXMSSMSSSMMSAAXAMSMMMMXSMSASXXSXMMXAXMAAXXASMAAXASM
XMSMSMAMAAAMMSASAAAMSSSXSAMXSAMMMSSMMSMMAMAXSXSXSAMAXSAMXMMSSMXAAXASMMMMMSMMXMAXMASASAMSMMSAMXXMXAAASMSSMMASAAXAXASAMAMMASMXSMSMMSSMMMSMSMSX
MXXMAXASMMSSXMASMMMAMXMAMXMAMAMXAAXMMAAXMSAXMSMASMMSASAXSMAAXAMXMSXXSMXSAAMMAMAMXMMXSAMXMAXMASMSMMMMSMXAMAMXSMSXMAMXMXASMMSAAAMXAXXASAMXAASA
MMSXMMMXMAAXMMXMMMSSMMMAMSMSMSMMSSSSSSMMXSXSMAMAMXAXASMMSMMSSSMSMAXMXSAMXSASASXSSXMASMMSXMMSMMAMXSAXXMXMAXXMAXSAMAMMAMXXSAMXMMMMMSSMMAXSMMMS
MASMSASXMMMSSMAAAAMAAXSSMSAAAAXXAAMAAAMSXMASMXMASMMMMMAAMAMMXXMASMAMAMMSMXAMAMAAXXSASAXMASMASMMMASXXAXMSMSSSMAXMXSMSAXMAMXMSMSXXMAMASXMASAAX
MXXAXAXMXAAMASXXMSSSMMMMAXSMSMSAMXMMMMMSAMAMAMAMXXAASAMXXAMMSSSMSXAMAMXAAMXMSMMMMXMAMXMSXASAMXSMAMMSSMXAAMAAXAXSAMXSASMSMAXAAMSAXASMMASAMMSM
MAMSMSMSXMXSAMXASMMMAMAMMMMXXXAMXAMAMAXSAMASAMSSSSSSSMAMSMSAAAAAMXMSASXMSMSAXXXXMAMXXSMXSXMMSMSMSXMAMASMSMSMMSAMXSXMAMAMSSXMXMXXMAXXXAMASAXM
MASAAMAAXSAMXSSXMASMMMAMXAMAXMMMSMXAMSXSAMAXAMXAAAXMXMAXAMXMMMMMMSXXMAXMMMSASXSMSMMMMSAMAXMAAAXAAMMMXMXXAMXMAXXMASAMMMXMAXXMSSMMASXMMSSXMASX
MXSMMMMMXMASAXAMSAMAASMMSASXSAXAAAMSXMASMMSMSSMMMMMMXSSSXSMMSSMXXSAMXMSMAAMAMAXAAMSSMXAMMSASMSMSMAASMSMMSSSMMSAMXXAMXSSMAMXSASAMXXAXAAMAMAXA
XMXMAXXAXSAMMXSAMAMXMMAAXAAASMMSXSXXAMAMXXMAAAMSAMASMAXAMMMXAAMSAXSAMXSMMSSSMMMSMAAAMSMMXMAXAXAXMSMMAAAXMAXAXSXXAMSMMAAMMMXXAXMMAMSMMMSAMASM
MAASXXMMXMAXSXMXMASMMMMMMXMAMMAXXMAMSMXMAAMMMSXMASAXMMMMMAAMSSMSAXXXMXMAXAAMXAAMXMMSMAAASMMMSMAMAXAMSSSMMMMSMMMSMMXAMSXMASMMSMAMXSXXXXSXXAXX
AMXMSXAMMMSMXMSMSASAXAXXMMMMSMMSSMAMMMMSMSXAAMMSMMXSAAASXXXXAAXMMMSMMASAMMSMMMMXAMAMXMXMMAAAMMXMAMXMXAMXMXMAAAAXXASXMAMSXSAAAMAMXAXXSAMXMXSM
XXAAMSSMAAMXAAAAMXSMMSAMXAAXAAASASXMAAMAAAXMMSAXAAAXXMSSMAXMSSMSAMXASMSMSMMMSMMMSMAXAXSASXMSSXSMMSXSXMSAMSSSSMMXMXSAXXAMAMMSSSMMSMSMSXSASAAX
MSMSMAMXMSSSXSMSSMMXMMXMSSSSMSMSAMXSSXSMSMMXAMXSMMSXXXAXMSMMXMASMSSXMAXMAMAAAAMAXMMMAXMAMAAAMXMAXXMMAXSASMMMXMXSAAXMMMSMAMXAXAMMAAAASAAASXSM
XAAXMMSXMAXXAXMAMAMMMXMXAXMXAMXMAMXAMXSXXXSAMXMXMAMMAMMMXAMMAMAMXXXASXMSMSMXSXMXSAXMSMMASMMMSMSSMMMSXMSSMASMMAAMMSMMSAAMSSMASMMSMSMSMSMAMAXX
MMMMAAMMXSMMSMMASAMAAASMMSSMXMMMAMXAXXSASAMXMASASASAAMAAXMMSASMXSAMMMAAXMAMXMXSXMAMSAMSAXAXMAXAMSAMMAMXXSAMAMMXSAXXAMXXSAMAAMXAXAMMXXXXAMMMS
MASMMMMXAXMAMASASXSMSXSAMAMXMMMSSMSMMXMAMMMSSMSAMXSXMMXSSXASXSMMMMSSSMMXXAMASAMXMAMMAXMXSSMMSMMSMSXMAMMMMXSXMAMMMXMSXSXMMSSMMXSSXMASMSMMMXAX
MAXAMXSMAXMAMMAMXASAMXSXMAMSXSAAAMAAXAMXMXAAAMMXMMMAMSAMXMMMAXAXSAMMAXAMSSXSAMXASXSMXMXAXAAAAAXXAAMASXMAMXSXMASAMSAXASXAXAXAXXMXMMXMASASMMMS
MSSMMAMMMMSSSMASMMMAMXXASMMSAMMXSASMMMSSSMMSSMAXMASAMMXMXMMSMSMMMASXSMXXAMXXAMXXSASAASMSSSMMSMMMMMSAMASASMXASXXAXSAMAMSSMMSXMSSSMMAMSMAMAAXX
XAAXMASASMMAAXSMMSMSMMSMMSAMXMAMAAAAAASMXMMMAMASMMMMSAMXMAAAXAAXSAMAXAXMMMAMAMMSMAMSMMAXAXXMAMMAMXMASXSAAASAMMMSMMAMMMMXXXMASAAAXMASMMASXMMM
ASMMSXSXMAMSMMMAMXSAAMASMMMSXXAMMMMSMMSAASASXMASMASAXMASMMSXSXSMMXMSMSMMXMMSMMMXMAMXXMSMXMASMMXSMMSAMMMMMMMAMAXXASXMSAMXMMMXMMSMMMXXAXAXAMXM
MXXAXXMASMMXSASAMXSSXSASXAXMASXSXMMXXSMMMSASAMAMXAMXSSXXAAXASAMXSAMAXAASMXXAMAMASMSMSXXAMAMAAMAXAMSAMXAMXMSSMMSSMMMMSASMSASXSAMAMSMMSMSSMMAM
MSSMMXSASAASMMSSMAMAXMAXMMSXXMXAAMAAMXXAXMAMXMSSMXSAMXMSMMMAMAMXSASMSSSMAMSASASMXMAASAMSASXSAMAMAMMASMASMXAXAAXASAAXSAMXMMAMMXXAMASAXAMXXSAS
XAAAAAMXSMMSSXXMMXSXMASMSXMASMMSSMMSSMSSSSXSAXXMXAMXXAMASAMXMAMMSAMXMMAMSXXASAMXSMMMMAMXSMAXAMMSMMSAMMXMAMXSMSSMMSAMMMSXSAASXMSSMMMXSAMAASAS
MSSMMSSMXXAMAXMXXAXAMXXAXASMAAXAMMXAAAMAXAASMMSMXMMSSMSASMMMSMSAMMSXMSAMXMMXMXMXAAXAMXMXXXAMASAAAMMASXMMSAMXXAAAXMXSXAXMASXSXAMAAXXAXAMXMMAM
AXAXMXAXSMSSMSMMMMSMMMXXSAMSMMMSSSSSMMMMMMMMXAAXXAAAAAMXMAMAAMMXMAMAXXMXAMAXMASXSMMSSMMXSMSSXMXSMMMAMAAAMXXXASXMMMAMMSMMAMMMASMAMMMMSSMMXSAS
SMSMSXMMMAMXXSAMAAAXAMSAMXAXXMAMXMAAMXAXXSAMMSSMSMMSMMMXSAMMXSAMXAXMMSMSAMXAXXMAMAAMAAAXXAXMASAMMSMMSMMSSSMMMMSAAMAMAMAMASASAMXXMAXAAAAAXSAM
XAXASASAMSMMMSAMSSXSAMASXMAXXMAXSMSMMSMSXSASAMAMXXXAXAXXMAXMAMASXMSAAAXMMMXSMXMAMMMSSSMXMSMMMMXSAAAXXXXAAAXSAAMMSMMXXXAMAXAMXXMXSXXMSXMMXMAM
MAMAMSMXSAAAASAMAMAMMSMXXXMASMXSAAAXXMASXXXMMSXMSMMXSSSSSSSMMSXMAAAMMMSSSSMAAASASAXXAAXAXXMASMMMXSSMMMMMSMMSMXSXMMSAXXSMXMXXXAXAASXXAMMMASXM
MXMAMASXSXXMXSAMAMXMXAMMASMMSASAMSMSSSMXASXSAMXMASAMXAAAAAAMAMXSMMMSXXAAAASMSMSSSXSMMXSXMAXASXAXXMAMAXAXAAXXMASAMAMMSAXAMSAMMAMSAMXMMASMXMMM
MSSXSASXMAXSXSXMAMXXXAXSASAAMMMMMAXSAAMAMAAMAMXXAMMAMMMMMMMMAXXSAMAMXSXMSAMXXMXMMMXAMAMAMSMMSMSMAMSXMXSXXAMXXAMAMXAAMXMAAMASAXMMMSXASAMXAAAX
MXXXMAMAMAXSAMXSXXMXSSMMASMMXAMXSXMMSMMAXMSXMMSMSXSSXXAAXSASXSASXMXSAMXXMASMSMAXAAXMMAXAAAAXXAASMMAASAMASMXMMXSAMSMXSXMAMSAMMSSXMAXMMMSSSMMA
XMXMASMMMXMMAMXXAASXXXAMAMAMSMSMMAAMMXXXMXXAMXAAAASAMSMSSMAMAAMMXMMMXSMXSAMAXMASXSSSSSSMXSSMMSMAMMXAMAMMASAXXMXMAMXASASAXMASAXAMAMMXAMXAAASX
SAXASXAXMAASXMMMXMMAAXXMMXXMAXAAMXSMMSSSMASMMSMSMMMAMAMMMMAMAMXMAMSSXAAAMMMSMMAMAAAAAAAAAXAMAAMMXSAMSMMSAMMMSAMXSAMXMAMMXMMMXMXXAMXXMMSMSMMA
XXAXAMXMSSXMASAMASXMMMSAMSSMSSSXSAMAMXASMAMMASAXMASXMASMASASAXXSAAAMSMXMXSXMXMSSMMMMMMMMMXAASXSAAMAMXMAMAMAAMAMAMAAMMSMMASAAMSASXXSAXAXXAAXS
MMSSSMSXAXAMASASMMAMMXMAMAAAAAMXMXSAMMMMMSSMAMMMMXAXSAXMAMXSXMXAMMXXXMAXXAASXXXAMXMASXMASMMMMASAMSSMMMAXSAMXSMMSXSMSAXAMAAMMSAASAASXMAXXXMXM
SXAAAAAMSSMMAXMMMSAMMXSMMSSMMMMXSXSXMAXMXMAMMSMXMAMXMAMMMMXSAMXXXMXMASXSMMMMMSSMMXMASASMMASMMAMAMAMAAXASAXXXMASMAMSMASXMXSXXXMMMMMMXMASMAMAS
AMMSMXMAMAXMXMSAMMXMMAMMMMAMXXSMMASMSSSMASXMAMXAXSXMSXSAXSAMAMASMMMXAMXMXAMAAAAXAAMMSXMXSMMMMSXXMAMSMSSMMMSMSXMMAMAMMXMXAMXMMXXSXAXMAAAMAMAS
MXXXMAXMMAMXSXMASMSMMASAXSAMXAMAMAMMAAAXAMMMASMMXXAMAASAMMASAMAXAAAMSSMMXSSMMSMMSASASXMXSAMAAMXSMMXAMXAAXAXAXMAMSSMMMAMMSSMAAXXMAMXSMXXMXMXS
XXSASXSSMSSXMXSAMXAASASXXAMXXMSSMMSMMMMMMXASAMMMAMMMMXMMMSAMAMSSMMMSAAAASMXXMAXAAMMMMAMAXAXMMSXMASXMSSSMMXMAMAXMAMAAMXSAMAASMMSMXAAXMAXMAAAX
MMSASAAAAAXMSAMASXXMMAXMXMSASXAXAAMASXXMASMMMSSMMSSMMSAAAMASXMAAMMSMMXMMSXAXSSMMMXAAMMMAXSMSASAMAMAAXMAMSAMXSMXMAXMMSAMXSXMAAASXAMASXAAAXMMS
MAMXMMSMMMSAMXSAMAMSSSXSAAAMAMXSAMXASXXMASAAAAMAAAAAAXXMXSAMMSXMMXAAXSAMXMAMMXMAXMSSSSMMMMAMASAMASMMMXSASXSAXMASMXSAMASXMMMMMMMAMSAAMMSXSAXX
MXSXSXAXAMAXXXMAXAAXAAAAMSMMMSXMAAMXSXXMASXMMSSMMSMMMSMMMMASASAXXSXSMAMSSMMXSAMMXMAAMMASAMXMMMXMMXXMASMMMXMMMSMSAXXAMSXMAAXAXXSMMMXSXAAAMAMX
AMAXMXMSSSMSMMMSXSSMMMMMXAASXMASMMSXMMMSAMAAXXXXXMAXMAMAAXMMASAMXMMXMXMAXMAMXASAXXMXMMASMSMSMMXSMSSSSXXAMMXSAMMMXMSAMXASMMSXMXSXSXMAAMMSMMXX
MMMXSAXMAAAAAAMMAMXMXXSXSSSMASMMMAMAAAMMMSXMAMMAMSMMSSSSSSMMAMMMAAAAMAMXSMMSSMMMSSXAXMAXXMAAXXAASAAXMAXAMSAMXSXMXAMXMSAMXASXSMMMMXXMXXAMASMA
XAAAXMMMMMSMSSSMSMASMASXXXAXAMXAMAXMXMMAMAAMXAAMXSAAAXMXMAMMAXAXXMMAMSXXSAAXAXAMXMASXSMMMMSMSMXSMMSMMMMMAMXSAMAMMMSAMXSMMXXMAAAAMXSXMAXSSMAX
SMMSMXAAXMAXXXXAMXAMMAMXASXMSMSSSSSXSXSMSSXMASXSXSMMSXSMSAMSMMSAMXXSAMXASAMXMSXSAMXMAXAASXMAXXMMXSAMXASAAXASASASAAMASAMXMMMAXSMSAAMASMXMXASX
XAXMASXSMAMMSMMXMMSMMASXMMSMMAAAAAMAXMMMAXXMXXASAMXXMAMAMSMXMAXXMAMXASAMXAXXXAMMAMAMXMSASAMMMSSSSSXMMASMSMMMMAAMMSMASXSAAAAMXAMXMXXMAMAMXSMX
MAMMMMAAMXAXAAXAXAMASMAXMASMMMMSMMMMSASMMSSMAMSMSMAXMAMXAMMAMAMXXSAMXMXMXMMAMMMSAMXXXMMMSMMMXMAMAMMSMXMAMAXAAMXXXXMAMAMXSXSXMMMMMSMSASAXSAMX
MMSXMMSMMSASMSSSMASXMAAXMASMAMXXAXAAMXMAXAAXXMMAMSMSSXMMSXSAMASMMMXMXMASXXSMMSASASMSMXAAXXASXMAMAXAAMAMXXMSSXXAASMMAAAXXXAMMAMXAXAAMAMAXSASM
XMAAXAMAMXMSXAAXMXMAAMMSMSMMAMSSMMMSSMSXMXSMXMMMMAXAMXXAAAXMSASAAAAMAMXSMAMMAMASAMXAMSAMASXSASXSMSSMASXMXMAXAMXMMXSXSASAMAMXXASMSMSMSMSMSAMA
XAMMMAXAMAAMMMMMSXSMMMAXXAMMSMMAAMAMAXMAMAXAMXAAMXMSSMMMXSXXMMSMSMMXASAMMMMMASMMMMSSXXAXXMASAMMXMAXXXMASXMAXXSAMSAMXMASMSAMASXXMAAAXMXXAMXMX
MMSSXMSMMSMMASXMXASMSMXSMXXAXMXSMMAXSMSAMMXXAMXXSSMAAXMAAXMSMASAMASMMMASXMAXMXAXXXAXMXMXXXXMASAAMSSSMSXMMMMXASAXXAXXMXMXSXSMMMASMSMXSAMXMMAM
XAAMAXAMAAASASAXMXMASAXAAMMSSMAMASXMXAMASAAXSSXXAMMSMMMMXSAMMASASASASXMMASMSSSMMMMXMXXMASXMSXAAXMXAAASMAMASMAMMMMAMXSXMXMASAASAMXAAAAASAMXAM
MMMSXMAMSMMMASAMSMMXMXMMSMAAAMASAMXASMMAMMSMMAMMAMAMAAXAAMAMXAXXMASMMAAMMMAAAAXAXMASMMMAMXAAXMASXMMMMMAXMAMMMMSMASXMAXMASXMMMMASMMXMSAAXXMMS
XMXMAMXMXXMMXMAMAASAMXSXMMSSSMAMAXMMMSMXMMAAMAMSAMASAMXMSXMMMSMMMAMMSSMMSMMMMMXSASMMAAMMSMMMSXXMXMXMMMSXMAXSAAAAMXAXMMXASASXXSAMXMMXXXMSMSMS
MXAMSMAXMAMXSSXMSSMAMAAAMAAMXMSSMMSMAXSXXXSMMSXSXSXAXMAXXAASAMAAMMAAXXMAMMXMAXXMAXXSSMSMAAMSXMAMMXMSAAAXSAAMMMXXXXMMXSMMMAAAMMXMAXMAXMXAAAAX
SSMSMMSASAMXMAMMXMMSSMMSMSMMSXMAMAMMMMMMSMAAXXAMXMAXAASASXMMSSSMSSMXSMMASXSMMMSMAMMXMASMMSSMAMAMAAASMSSMMMSXSASMSMXMASAMMSMMMSMXMAMXXAXMSMSM
XASAMXMAAASXMSSSSMMXAAXAAXXMXASAMXMAAAMAAMSMMMAMMMMSSSMAAXMXXMAAAMMMMXMSAMXAAAAMMXMAMMMAMMMMSMAMSSXXMAAMXAAAMAMAAXXSASAMAAAXSAAASXSAASXMMXAA
SMMMSAMMMMAAXMAXAASXSMMMMMAMMAMXSXSSSSSSSXXXASAMAXXAXXMXMSMSSMMMMXASAAXAAASXMSMMXAMMSXMAMMAAMXAMXXMAMSMMMSSXMAMMMMXMMMAMSMSXSMXMSASAMMAASMSS
AXAASASAAMSMMMMSSMMAXAAXASAMXSMASAMXAMAAMASMMSASXSMMXSAAXAMAMXSASAMXASMSAMXXXXXSSMMMAMSMSMMMSMSMSASXMXAMAMXAMMSAAAMSXSAMMXXAMXSAMMMASXMMMAAM
MMMMXXMXXMASAXXAMAMSMSASMXSMMXAXMASMMMMMMAMXMXXMMXASASMSXSMSXMMASAXXXMAMXSMXSAXMASAMAXAMMMMSXAAAXAMMAMAMSSSXMAXXMXAAXMASXMMAMSAXMASAMXASMMMX
XMSMSSSMMSASMSMMSSMAAMMMXMAXAMMMSSXMXAMXASXMMSXMXSXMASAMAXAMXSMMMMMSSSMSAMXMMMMSAMXMMXMXAAMXMMMSMMAXMSSMXAXMXMSAMMMSAAAMAXMAMXMXSXXAMXMXAXAS
XSAAXAAAAMMMMXAXAASMSMSAASXMMMAMXMASXXSAAMAAASAMAMXMXMASXMAMAXAAAXXAAAAMMMAMAMXMASAMSASXSSSMAMMXAMAXMAAXMMMSAMAMXXMAMMAMAMSMMXXMMMSMMMMSSMAM
MSMSMSMMMSSMSMSMSXMXXAMSMSXAASMSMSAMXAMXXMXMAXAMSSSMXMXMASAMXSXMSSMMXMMMMMMMAMASXMAXAAMAMAMMAMXMAMAXMMSMASAMASMSAMSAMSXMXXAXMSSMAAAMMXAAXMAM
AMAMAXXXAAAMAAAAXXMSMMMMMSMSMSAAXMASMSMAMSMMXSAMXMAMMSMMMMMSMMMAMMMSAAAXSASXXMAMMXSXMSMAMAMMXSSXSMSXXXMMMMASAMXMAMSAMAASXSXMAAAMXSSSXMMSXSSS
MMAMXMMMMMSMMMMMMMMXAMXAASMMAMMMMMSMXMAMMAXAMXXMASAMMASAMMAMAAAXXSAMMMMXMASAMMSMMAXAMMMXSXSMSMMAMAMXMASAMSXMXSAMAMSAMSMXAMAMMSSMMMMAXMAMASMM
XSMSSSMMSAMXXXSSMMSSSMSMMSAMSMXSAMXMXMAMSSMMMAMSAXAMSASMXSASMMSMXMXXASMMSAMAMAXAMASXMAMXMMAXMAMAMAMXAMMXMXAAMSASAMSXMMAMSSMMAAMMAAXMAMMSXMAS
AXAMXXAAMAMSSXMAAAAXAAAXAMAMMMMMAMAMMMMMXXAAMAMMXXAMMAMMASAXXMAMXSSSMSAAMXMXMAXAMXSAMASAMSMMMAMXSXMMSSMASXMMMSAMMXMAMMAMAAXMMSSSSSSXSAASASAM
MMMMMSMMXAMXXASXMMXMMMMMXSMMMAASASASMSASASMMSASXSAMSMSMMXSAMSSMSAMXAMSMMSMMSMSSMSAMXSMSXMAMASASXAAAAAAMMMMSAMMAMXAXMASASMMMMSXAAAXAAMMXXAMMS
MAAAAAMMMSSMSXMXXXSMSAXXAAAASMMSASAMASASXAXASASAMSMAAAAXAMAMXAAMMSSSMSAAXAXSAXAAAMMMMAMMSXSAAMAMMMMMMSAMSASASXMAMMMSXSASMMMXAXMMSSMXMSSMAXMX
SSMSSSXSAAAAXMSAMXAXAASMMSSMSMMMMMMMMMAMMMSXSAMXMAMMMMXMMSAMXMMMAMAAASMMSSMMAMMMMXAMSMSASAMXXSAMXAMMMMASMASXMASMXSASMMAMAASMMXSAAAXSXAXMMMMM
MMAAXAAMMSMMMXSXAXMASMMMXAXXXXMAMAAMMMMMAXMAMMMXSSMSASAAMMASXMXMMSMMMMMAMAXMAXSSXMXXAAMXMXXSMMAMSASAAMSMMMMMMMMAAMAMXMSMSMSAXAMMXMMMMXXAMAAA
AMXMSMMMXAXAXAXXXAXAMAAXMASXMMSMSSSMXAMMSAMSMMMAMAASASMSMMMMAAXSMMXXAXMASMSSSSXXAMAMMSMSAMXMASAMXAXMMSXMAMXSASMMMMAMAAXAXMSAMXSAASMMAMSXSASX
XXAXAMXXSMMMMMSMSMMMSMMMMMMXMAAMMMAASAMAXXXAAAMXSMMMAMMMMASMMMMSAMASMXSASAAAXAMXAMAAMMMMAAAXMMAMMSMSMXMSMSAXAXSAMXXASAMXMASASAMMXMAMMXAAXAAM
MSMMMSXMXXMSAMAMAXMASXMAXAXXMSSSMMMMMAMMSMSSSMXASMSMXMAMSAXAAMAXAMMSMAMAMAMXMAASXSSSMAASXSMSXSSMAAAXMAMAAMMSMSAMXASXMXSASAMXMASMSSMMSMMMMXMA
MAXXAMMMAAXASXSSSMMASASMSMXSAAAAXSMXSXMXMAXMAMMASAAMXSAMMSSSMMSSXMXXMAMSMSMAAXAAAXMAXMMSXAMAMAXMSMMMMMMMMMXAXMAMXMMXMMXMASXMMXXAAMAXSAMXSAXM
SASMAMSMMSMXMMMAXXMAXMMAAASMMMSMMSMAXSMAMSMXAXMAMXAMAMXXAAMASAMAMSXSMMSAAAMMSSMMXMSAMMMMXXMASMMMMAAAXAXMASXMMSASAMXAXAMSAMSAXAMMMSSMSAMAXAMX
MASXMAAXAAMASMMAMXMSSMMMMMMAAMAXAMMMXMMASAMMAMMXSSSMXSAMMMSAMXMAMMAAASXMSMSMXMASMAMASMAMXXXAMMAAMMSMSXSAAXAAAXXXXMXAXSMSAAASMXSMAAAMSXMXSMSS
XMXAMSSSSMSASAMMXAAAAASMSMSMMSAMXSASASMMSAMSSMSSMAMMAXMXSXMXSAMMXMMMMMAMAAAMMXMSAMSAMMAMMSMSSSSXSAAXXAAMASMMMSSXSMSSXXASMXSXMAXMMSMMMMSXAAAX
MMMXMXAAMXMXXMSASMMMSMMAMAAXXMASXSASASAXMMMMAAXAMAMMMSMXSAMAXMASMMSAXXSMMSXMXSXXMAMXSSMSAAAXAAAMMMMSMXMMASAXMXMAMAAXAMAXAXXAMSMMXMXSAAXSMMMS
AXMASMMXMMSSMXXMAMXAAAMSMMMXXXAMXSXMAMMSAXSSMMMASXSXMSAAMAMXSXAAAASXSMXXXMAMSMSAMXAAXAMMXSMMMMXMASXSXMXSASMMSAMAMMMSXMSMMMXAMAAXAMASMSXMMAMA
XSMMXXMMMXAASAMXMMSSSMMAMXSAMSSXMSAMXMXMXMXXAMSXSAAAMXMASXMMMXAMMMMASMXMXMAMAAAMXSXMMXMAXMAASASMAXXXAXXMASMASAMXMAAMAMMAMASXMSMMXSAXMMAMXMXA
MSASMMXAXMSMMXMAXXAAMASXSAMAMAMMMSAMXSASAMXSXMMAMMMSXMXMAMAASASMXMMXMMAXASASMSMMAAAXXSMSSSMMSASMMXSSMMMMAMMMSAMSSMMSAMMXMMMAAAMAMMMXMSAMMSMX
ASAMXAXSSXMASMSSSMMSMAMAMMMAMASXAXXMASASASAXAMMXMSAMASXMAMXMAAXAAXSMMSSMXSASAMAMXSAMAAAMXXXAMAMAAAXXSASMAMXAMAMAAAASMSMSMSSMSMMMSAMAAXAMXAAM
XMAMMSMAAMSAMAAAAXXXMAMMMXSXSASMMSXSAMXMAMXMMMAAAMAXAMXMASMXMAMSXMSAMAAAMMXMMSAMAXAMXMXMASMMMSSMMMSASXSSXSAXSMMSSMMMXAMXAMAXMAMMSASXSMMSSMSX
MSAMAAMMMMMSSSMSMMMMSXSAMAAAMASAAAMMXSSSSXSXMAXMSSSMSSXMMSAASXMMMXSSMMMMSAAXAMMSMXXMAMXMMSAMXXAXAXMXMAXAMMMMXMMAMMXSSMSMMMMXMAMXMXMAAAXAAMAX
XSAMSXSAXSAAXAAXXSAAXASMSSSXMASMMSMXMAXAAAMASMSMAAMAMXMXMSMMXAAMXAMXSXAAXXMSMSMXMAMXASXMASMMMMMMMMMAMSMMMAMSAXMXXSAMMAXMXSXSXXXAXMMSSMMSSMAS
ASAMXXXASMMXSXMMASAMSAMXAMAXMASMAAASMMMMMMMAMAAMMMMAMMMSMMXXSSMAMMSAMSMSMMMXXAMAMAMSAMAMASXMAAXAXXXAXASMMAMMXXMXMMASMMXMASXSAXSMSAMAMAAXXMAS
MMXXXMSMMAAMMXXMAMAXMMMMSSMSMMSMMXAXAAAXAAMASXSMXSSSMSAXAXSAXASAAXMAXXMAMAMXSAXAXAMXAMXMASAMSXSAASMMSAMSMSMMSSXMASAMAXAMAXAMXMAAXAMXMSMMAMAS
XAXSMAAMMXMAAASMXSAMXSAMXAXSAMXMAXSSMSAMMXMMXAAAAAAXAMMSXMMMSAMMSAXMMXXAXMSAMMSSSSMSSMXSASMXAAMXMXAAMMMSMXAMXAASXMMSAAXMMSXSASMMMSXMAMASXMAS
MMSMASMXXAXMXMXAAXASASASXSMSAMAMMSMAXMASMSSSMSMMMSMMSMXXMAAAAXXAAAASASXSSMMASAAAAAAXMAXMASXMMMMAXMMMMXXMASXMMSMMAMMXMASMXAMMASMMMAASASAMXMAX
MXAXMAMXSMSAMXMMMSMMASAMXXAXAMXAMMXMMMMAAAAXMAMXXXXAAMXSAXASMSASMXMMASMMAMSAMXSMXMMMASXMAMMXAASXSXAXMXXMXMXSXAXSXMAAXMAMMXSMAMAAMMAMAMXMXMMM
SSSSXSXMAAAMAAMAXXAMAMMMMMAMSMSSSMASASASMMMMSMSSXAMSSSMMSXAAAXMAMAXMAMASAMXXMAXXXSSSMAMSXSAMSXMAMXMMSMASAXXXXMMMMSSSSMAMXSXMASMMSXAMXMAMAAAA
XAXMAMSAMXMSSXSAXXMMXSMMASAMAAAXAXMSASAXMSSMSXAMAMXMAMAAXMMMXMXXXMMMMAMXXMASMASXAMAAXMMMAMXXMAMXMMSAAAMSASXSMSAAAMAAXXASXMAMAAAXMMMMAMAMXXAS
MAMAASAMXSXAMAMXSMMAXAASASAXXMMXSMMMMMXMASAASMAXAMXXAMSMMAXAMSMSASMSMSXSAMXXMASXMMXMMXAMAXMXMAAXMAMMXSAMAMXSASMMSSMSMSMSAMAMSSMASXMSSSSSMSAA
MXASXSASAXMASMAMAAMMSSMMXSMMSASXAMSSSMSAXSMMMSASMXSMXXAASAMXSAAMAMAAAAXAMXSXMXSASXSSXSASAMXMSASMMSSMSXMMAMXMAMAXAAXMASAMXSMMAAAAAAAAXAAAAXAM
MXMMASAMXMAXAAAAXXMAAAMMMXMASAMSMMAAAAMSXMXXAMXXAASAMSSMMSMXMMSMSMXMMMSXMASMMAMAMAAXMMAMXAMXMSAAXMAMSAXSXSASASAMSSMSSXAXAAXMSMMMSMMMSMMMMXAX
XMMMMMMMAASAMSSSSMMMSXXXXXMXMAMMXMMSMMMMMMAMXSSMMMMAMMAMAAXXMXAXMASXXAAAMAMAMXMSMMMMXAAMXSAMXMXSMSSMSXMXMSMSAMMXXMAXMMMMMSSXXXMAXXXXAXMMSSMM
MAXSSXMSXSMMXMAAAXXXMMSMSMXXSAMXXMXMAXXAAMXSAAMMXXMSMMAMMSMMSMSMSMAMMMSXMSMSAMXXAXMSSMXSAMXMXSAMXMXAMMXAMMAMMMSXXMSMSMMAXAXAMXMMSMMXMMMAAAAX
ASMXSAAXMXMASMMSMMXMAXAAMSAXMXMASXMXMASMMSAMMSMMXSXXASMMXAXMAAAAAAAXAXMMMXAAMMMSMMSAMAMMXMAXAMMXXMMSMMSSSMAMAXXAXXAAXMASXXMSMXSAAAASMSMMSSSM
MXMMSMMSXSMAMSAXAMXMSSMMMXXSMAMAMAMSSMSAAMXMMMAMASMAMMAASAXMAXMSMSMSMXMAAMXMMAXAAXMAMSMAAMMMMSSMSMAAAXSAMXMMAMXMMSMSMMAXAMXAXAMSSSMMAAMAMAMA
ASAMXMASMMMSSMSSSSSMAAMSMMMAMAMAMAMMAXXMXMMMAXXMAMMAMAXMMASXSMAXAAAAASXSMSMSSXSMSXSXMAMSMSASAAAXXMSSSMMAMXXMMSAXMXXAAMXSAMXMMMMMXAMMXMMMMMMA
XSAMAMAXAXMMAAASAAAMSSMAAAAMSAXSXSMXMSMSAMSSMXAMXSMXMXSMXXMAMXAMSMSMSMAAXMAMXAXXMASMSXMAXMAMMSMSAMAAMASXMASXMSASXMMSSMMAMMAXAAAMSMMSMMMXASXS
ASAMXMAXSMMSMMMMMSMMMAXSSMMXMAXXAMSMXAMXASXAAASXAMASXMMAXMAMXAXAXMXXMMSMMMMMXXSSMMMXMXAMMMSMMAXSAMMSMXAMMAXMAMAMXAAAAAXXMSXMXXXXAMXAAXASXSAM
XSAMXMMMMAXSXSSXXAMAMXAMXAASMMSMAMASMASXMMXMMMXAMMAMAASMXXMMXSSSXMMXMAMXMSAMXXMAMSXMAMXAMMXASMXSAMXXMMMAMASXXMMMSMMSSSMMASXSASXMSAMMSMMXMMAM
MSAMMAMASXMXAAMXMSMMSMMSMSMSAAXMXMAMXAMXMAMXAXAMXMSSSMMMMSMMAMAMAAAAMXSAASASMXSAMMAMAASXSMSMMXMSXMXXAAXXSASMASMAMAMAMXAXAXAMASMAAMAMMXMAAMXM
ASMMAXSASAMMSMMSMASAAXXAAAASMMMSMMMSAMXAMASMMMMMXSAMXMAXAAAMXSMSXMSXXAMXMMXAAXAASAMXMXMAAASMSMMMAMASMMSXMAMMAMMAMMMSMMSMMSMMAMMSMXSXMASXMXAX
XXMASXMXSAMMAXAAXMASMSAMXMXMASXMAXAXAMSSXXXAMASAMXMXASMMSSSMAMXMMMAAMSAMXMSSSMMXMAMXSAMXMAMAXXASAMXXAAMAMAMMXSXMXSAMAAXMXSXAMXAXXAMMSMMAMSXM
AAXXAMMASXSSMMSSXSAAXMAXXXAAXMASAMSSSMAMMSSXMASAASXMXSAAAMAMSMAMAMMXMAAAXAAAXMXSSMMASASAMSMXMSXSMXSXMMMXMAMMMSAAXMASMMXSAMASXMASMXXMAMMAXMAM
SXMASAMASAMAAAAAAMSMSAMXSSMXSXXMAMMAMMXMAMAXMMSMMSAXMSMMMSXMAAMMSSXASXMMMMXSMMAAAAMMSAMXSXSAASAMMASMAAXXMXSAAXMMMMMMAMSMMSAMAMSXMAXSMXSMXMAA
XMAAAXMASMSSMMMMMMMXSAMXAXMASMSSSMMAMSMMSSMXXMXMXSAMAMXMASASMMXAAMXXMAAXXMAMAMXSSMMMMMMXMASMAMAMMASASMSMSASMSSXMASASAMAAAMASXMXAMSXMXXAMXSXM
MAMXXXSXSXMMXAMASXMASAMXMXSAXAMAAXMAMMMAMAXSMMMMMMSAMXAXMXXAXAMSSSSSSSMMAMAMSMMXMASAAAMAMXMMASAMMMSAXAXXMAMAAXMXMSASXSMMMSAMXSMMMXAMASASMMSM
ASMMSASAMMMMSXSASAXXXMAXXMMAMSMSMSSSMSMSXMMMAAXMAAAMMXMSMSMMMMMAAMAXAXMSASXSAMMASXMMSMSMSMXSASASXXMAMXMMMSMMMSSMMMXMAXXAXMASAXXXAMAMXSAMMAAX
MXAASAMAMAAXMXMASMMMSAMXMSSSMAMMAMAMASAMASMSSMSSMXMMSAAXAASAAMMMSMMMASXMASXAAXMASAXAMXAAMAXMMSAMXAMMMASAAXASAMXASXSMSMSMSSMMMMXMSSSMXMAMMSSX
XSMMMASMMSSXSAMXMAAMASXXMAAASXMMSMAMMMASAMXAASAXXXSASMMMSMSMSSXXAMXMXMASXMASMMMASXMASXMMMXSAAMAMSMAAAASMXSAMSSSMMASAAAAMMAXMAMXAXAAMAMAMXAXX
AXXXSXMAXXMXSSSMSSMSXMSSMMSMMMXXXAMXSMXMXMMSSMSMSAMXSAMXXXXMMMMAMXXXSSXMXSAMXXMXMXAXXMASXMSMMSMMSXSAMMSXMMMSXMAXMAMXMSMXSAMSXSXSMSMMXSASMASX"
in print_int (count_xmas (String.split_on_char '\n' str)); print_endline ""
