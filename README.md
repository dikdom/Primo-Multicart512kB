# Primo-Multicart512kB
This is the repository of the 512kB MultiCart for the Hungarian 8bit computer Primo. The https://oshwlab.com/vass.sanyi/primocart link also contains 
the hardware source, but you may get it directly from here (which you have to import into your EasyEDA editor).
This repo contains all the necessary information to build one (or more) cartridges for yourself.

Ez a repo a Primo 512kB-os Multicart összes forrását és gyártásához szükséges file-okat tartalmazza. A hardware forrásokat a 
https://oshwlab.com/vass.sanyi/primocart URL -en is megtalálhatod, de innen is letöltheted (csak utána be kell importálni az EasyEDA editorba).

A binaries könyvtár tartalma:
- mcromgen.exe - ez a lefordított eeprom kép-generátor program, windows alatt fut. A bemenete egy helyesen kitöltött .plst file - aminek egy 
templétje szintén ebben a könyvtárban található. A .plst file formátumát a template-ben megtalálhatod.
- firmware-boot.bin - ez bináris a lefordított firmware, ami gyakorlatilag elindul, kirajzolja a .plst file által leírt menüt és elindítja a kiválasztptt
programot / ROM -ot.
- Két példa image, amik már egy-egy legenerált, teljes ROM image -ek. Ezek egyikét beleírva az 512kB -os EEPROM -ba, a kártya teljesen működőképes. (OG és VG a lista összeállítóinak a monogrammja). A két példa egyébként nagyrészt átfedésben vannak, nem készült ám olyan sok játék Primora...

A gyártáshoz a gerber file mellett megtalálható a BOM és a placement file-ok is. A lapok szélein a furatok az SMT beültetés miatt vannak, illetve az élcsatlakozó alatt ott van a 'JLCJLCJLC' string is, ami a gyártási szám helyét jelöli (nekik kell, nem nekünk). Felforrasztott élcsatlakozóval ez az ID nem fog látszódni.

Ha magadnak készítesz listát, akkor a következő képpen kell megírnod az EEPROM-ot:
- $0000 címtől kezdve kell rá felírni a firmware-boot.bin tartalmát
- $1000 címtől kezdve kell rá felírni az mcromgen által generált bináris file-t (aminek kiterjesztése alapesetben .mcrom)

A sources könyvtár tartalma:
- firmware - ebben a könyvtárban van a firmware-boot.bin file forrása. Én ezt anno az ASM80.COM címen található online z80 IDE -vel készítettem, nem tudom, h most elérhető-e.
- hardware - Az exportált kapcsolási rajz és nyákterv az EasyEDA online IDE -jéből. Ez az, ami megtalálható a fenti oshwlab webcímen is.
- imagebuild - Ez az imagebuilder pascal forrása. Ezt Lazarusban készítettem. Ha akarod, magadnak lefordíthatod Linux -ra, Max -re is. Win32-re fordított futtatható bináris megtalálható a binaries könvytárban.
