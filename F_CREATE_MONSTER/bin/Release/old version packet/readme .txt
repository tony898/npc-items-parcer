need help fixing this
items are not outputing in the out file because the old packet has 00 bytes and the new packets dont so code needs altering


items get parced
from 07 which is the count of how many items there is 7 items in the packet
 which starts in the vb code  from   Dim item_count = Convert.ToInt32(myBytes(i), 16)

and in the c++ code it starts from    if(!item) return true;




here is old packet the items are near to the end of theses packets starting from item count which is 00 07  so need to remove the code that adds the 00 that ive spaced



[<F_CREATE_MONSTER>(0x72)<141>]04 2B 00 00 09 49 1E 96 00 0C E5 E1 00 0E 53 F7 00 00 04 C2 37 02 40 00 00 00 00 00 00 00 03 00 03 E8 00 00 00 00 33 2C 00 05 00 11 04 0D 10 19 1C 00 45 6C 64 72 65 64 20 4B 72 65 62 73 5E 4D 00 00 00 00 03 00 48 01 0A 00 00 00 12 2C 00 04 2B 65 E1 D3 F7 1E 96 64 00 6A 00 00 00 00 03 49 09 04 2B             00 07   00    00 0A 06 50      00    00 0B 16 82 01    00    14 0C 16   00   6A 00    00 00     00 15 0C 18      00   00 16 0C 17   00   00 19 0C 1A   00     00 1C 09 B8 00

new packet that only parces the npc and not the items 
[<F_CREATE_MONSTER>(0x72)<135>]00 84 72 03 FC 00 00 09 49 1E 96 00 0C E5 E1 00 0E 53 F7 00 00 04 C2 37 02 40 00 00 00 00 00 00 00 03 00 03 E8 00 00 00 00 33 2C 00 05 00 11 04 0D 10 19 1C 00 45 6C 64 72 65 64 20 4B 72 65 62 73  5E 4D 00 00 00 00 03 00 48 01 0A 00 00 00 12 23 00 03 FC 65 E1 D3 F7 1E 96 64 00 6A 00 00 00 00 03 49 09 03 FC   00 07         00 0A 06 50            00 0B 16 82 01          14 0C 16        6A 00              00 15 0C 18           00 16 0C 17        00 19 0C 1A          00 1C 09 B8 00





