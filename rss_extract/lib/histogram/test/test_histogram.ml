open OUnit2

let test_counts = [
(0, [("\x63\x68\x72\x69\x73\x74\x69\x61\x6e\x69\x74\x79", 1800);("\x69\x6e", 9761);("\x74\x68\x69\x73", 4822);("\x6f\x66", 14108);("\x77\x65", 4773);("\x63\x6f\x6e\x74\x69\x6e\x75\x65", 213);("\x6f\x75\x72", 4039);("\x6c\x69\x66\x65", 1469);("\x61\x6e\x64", 22209);("\x6c\x65\x61\x72\x6e", 718);("\x74\x68\x61\x74", 5243);("\x69\x73", 7563);("\x77\x69\x74\x68\x6f\x75\x74", 221);("\x6d\x75\x73\x74", 136);("\x61", 12943);("\x6f\x77\x6e", 692);("\x73\x6f", 1720);("\x64\x6f", 1633);("\x75\x73", 3099);("\x77\x68\x65\x6e", 1687);("\x66\x72\x6f\x6d", 2906)]);
(2, [("\x62\x79", 491);("\x69\x73", 1285);("\x74\x68\x65", 6284);("\x77\x68\x79", 127);("\x77\x69\x6c\x6c", 597);("\x74\x6f", 2658);("\x63\x65\x6e\x74\x72\x61\x6c", 306);("\x61\x6e\x64", 4254);("\x79\x6f\x75", 519);("\x68\x61\x76\x65", 190);("\x6d\x69\x6b\x65", 174);("\x6f\x66", 2911);("\x68\x6f\x73\x74", 383);("\x74\x61\x6c\x6b", 688);("\x61\x62\x6f\x75\x74", 554);("\x69\x6e", 2399);("\x68\x6f\x77", 306);("\x6f\x6e", 2257);("\x6e\x65\x77", 135);("\x63\x61\x6e", 228);("\x79\x6f\x75\x72", 388)]);
(4, [("\x74\x68\x65", 537);("\x61", 388);("\x61\x6e\x64", 358);("\x69\x6e", 250);("\x2d", 695);("\xd8\xb1\xd8\xa7", 377);("\xd9\x88", 848);("\xd8\xaf\xd8\xb1", 1170);("\x6f\x66", 359);("\xd8\xa8\xd9\x87", 720);("\x77\x69\x74\x68", 111);("\xd8\xa8\xd8\xa7", 256);("\x66\x72\x6f\x6d", 121);("\xda\xaf\xd8\xb2\xd8\xa7\xd8\xb1\xd8\xb4", 114);("\xd9\x85\xd8\xa7", 114);("\xd8\xa7\xd9\x81\xd8\xba\xd8\xa7\xd9\x86\xd8\xb3\xd8\xaa\xd8\xa7\xd9\x86", 127);("\x6e\x65\x77", 152);("\xd8\xa7\xd8\xb2", 603);("\x74\x6f", 266);("\xd8\xa7\xd8\xb3\xd8\xaa", 133);("\x6f\x6e", 314)]);
(5, [("\x70\x65\x72\x73\x6f\x6e\x61\x6c\x20\x6a\x6f\x75\x72\x6e\x61\x6c\x73", 245);("\x6d\x79", 1428);("\x73\x75\x70\x70\x6f\x72\x74\x65\x72", 181);("\x6f\x66", 21481);("\x74\x68\x69\x73", 6850);("\x65\x70\x69\x73\x6f\x64\x65", 5430);("\x31", 355);("\x68\x69\x73\x74\x6f\x72\x79", 392);("\x6d\x65", 747);("\x61\x6e\x64", 30402);("\x6c\x69\x76\x69\x6e\x67", 326);("\x77\x69\x74\x68", 7956);("\x69\x6e", 14230);("\x65\x70\x69\x73\x6f\x64\x65\x2c", 636);("\x64\x72\x2e", 888);("\x69\x6e\x74\x65\x72\x76\x69\x65\x77\x73", 155);("\x66\x65\x6c\x6c\x6f\x77", 102);("\x69\x73", 8634);("\x66\x6f\x72\x6d\x65\x72", 252)]);
(6, [("\x6d\x75\x73\x69\x63", 176);("\x62\x79", 384);("\x6f\x66", 1607);("\x26", 419);("\x6d\x6f\x72\x65", 102);("\x69\x74", 265);("\x70\x6f\x64\x63\x61\x73\x74", 189);("\x74\x68\x65", 2786);("\x2d", 149);("\x74\x6f", 1687);("\x79\x6f\x75\x72", 126);("\x73\x68\x6f\x77", 192);("\x77\x69\x74\x68", 571);("\x79\x6f\x75", 295);("\x72\x61\x64\x69\x6f", 141);("\x6f\x6e", 1676);("\x6e\x65\x77", 262);("\x69\x6e", 1028);("\x65\x70\x69\x73\x6f\x64\x65", 836);("\x77\x68\x6f", 120);("\x69", 173)]);
(7, [("\x6d\x75\x73\x69\x63", 3789);("\x77\x65\x6c\x6c", 1212);("\x69", 5867);("\x68\x6f\x70\x65", 819);("\x79\x6f\x75\x72", 10286);("\x68\x61\x73", 2780);("\x75\x70", 3488);("\x73\x69\x6e\x63\x65", 370);("\x74\x68\x65", 74049);("\x6c\x61\x73\x74", 1288);("\x74\x69\x6d\x65", 2142);("\x79\x6f\x75", 15335);("\x77\x65\x72\x65", 752);("\x68\x65\x72\x65\x2e", 126);("\x69\x74\xe2\x80\x99\x73", 825);("\x6c\x6f\x6f\x6b\x69\x6e\x67", 407);("\x70\x72\x65\x74\x74\x79", 190);("\x77\x65\x6c\x63\x6f\x6d\x65", 779);("\x74\x6f", 39158);("\x61\x6c\x6c", 5362);("\x6e\x65\x77", 4770)]);
(8, [("\x74\x68\x65", 1280);("\x66\x72\x6f\x6d", 208);("\x6f\x66", 597);("\x73\x6b\x69\x69\x6e\x67", 134);("\x69\x6e", 372);("\x26", 112);("\x77\x68\x61\x74", 116);("\x61\x62\x6f\x75\x74", 244);("\x61", 566);("\x74\x6f", 883);("\x74\x68\x61\x74", 132);("\x68\x65", 114);("\x75\x73", 105);("\xe2\x80\x93", 170);("\x68\x6f\x77", 210);("\x66\x6f\x72", 394)]);
(9, [("\x74\x68\x65", 144)]);
(10, [("\x70\x65\x72\x73\x6f\x6e\x61\x6c\x20\x6a\x6f\x75\x72\x6e\x61\x6c\x73", 234);("\xe2\x80\x93", 632);("\x69\x6e", 1100);("\x2d", 1374);("\x77\x69\x6c\x6c", 134);("\x74\x68\x65", 2934);("\x66\x6f\x72", 541);("\x61\x6c\x6c", 183);("\x6f\x66", 1465);("\x6f\x75\x72", 250);("\x62\x75\x74", 159);("\x69", 281);("\x61\x6d", 132);("\x61\x72\x65", 248);("\x79\x6f\x75\x72", 277);("\x6f\x72", 212);("\x26", 139);("\x61\x73", 383);("\x68\x61\x76\x65", 178);("\x74\x6f", 1429);("\x62\x65", 245)]);
(13, [("\x69\x73", 137);("\x61", 284);("\x69\x6e", 231);("\x74\x68\x65", 672);("\x6f\x66", 414);("\x61\x6e\x64", 536);("\x6f\x6e", 176);("\x74\x6f", 353);("\x79\x6f\x75", 104);("\x61\x62\x6f\x75\x74", 120);("\x74\x68\x69\x73", 127);("\x66\x6f\x72", 162);("\x68\x65\x72", 101);("\x2d", 108)]);
(15, [("\x65\x70\x69\x73\x6f\x64\x65", 1632);("\x61\x6e\x64", 6738);("\x74\x68\x65", 14691);("\x6f\x66", 4784);("\x61\x6e", 957);("\x61", 4783);("\x69\x6e", 3762);("\x68\x69\x73", 509);("\x69\x66", 169);("\x73\x68\x65", 129);("\x77\x69\x74\x68", 2106);("\x6f\x76\x65\x72", 311);("\x68\x65\x72", 289);("\x6c\x61\x73\x74", 144);("\x77\x69\x6c\x6c", 788);("\x77\x68\x6f", 329);("\x69\x73", 2246);("\x77\x68\x65\x6e", 163);("\x77\x68\x61\x74", 554);("\x74\x6f", 4927);("\x6f\x75\x74", 464)]);
(16, [("\x6d\x75\x73\x69\x63", 287);("\x74\x68\x65", 568);("\x61\x6e\x64", 159);("\x2d", 419);("\x70\x61\x74\x63\x68\x77\x65\x72\x6b", 112);("\x77\x69\x74\x68", 155);("\x69\x6e", 180);("\x65\x70\x69\x73\x6f\x64\x65", 191);("\x74\x6f", 194);("\x26", 169);("\x68\x6f\x77", 115);("\x68\x69\x73", 138)]);
(17, [("\x61", 239);("\x74\x6f", 301);("\x61\x6e\x64", 114);("\x79\x6f\x75", 231);("\x69\x6e", 114);("\x74\x68\x65", 279)]);
(150, [("\x74\x65\x63\x68\x20\x6e\x65\x77\x73", 263);("\x66\x61\x63\x65\x62\x6f\x6f\x6b", 181);("\x74\x6f", 3450);("\x6d\x69\x67\x68\x74", 103);("\x62\x65", 524);("\x61\x6e\x64", 4450);("\x6f\x6e", 1561);("\x69\x74\xe2\x80\x99\x73", 209);("\x6f\x66", 3659);("\x61\x6c\x6c", 393);("\x74\x68\x65", 6222);("\x77\x61\x79", 108);("\x67\x65\x74", 440);("\x62\x61\x63\x6b", 104);("\x6e\x65\x77", 334);("\x28\x74\x68\x65", 268);("\x61\x70\x70\x6c\x65", 171);("\x69\x73", 2341);("\x66\x6f\x72", 955);("\x69\x6e", 2077);("\x61\x72\x65", 920)]);
(151, [("\x6d\x75\x73\x69\x63", 113);("\x64\x65", 177);("\x26", 300);("\x6e\x65\x77", 116);("\x61\x6e\x64", 241);("\x74\x68\x65", 366);("\x2d", 960);("\x6f\x66", 188);("\x66\x74\x2e", 115);("\x74\x6f", 205);("\x61", 181);("\xe2\x80\x93", 1058);("\x69\x6e", 123);("\x79\x6f\x75", 103)]);
(152, [("\x6a\x75\x64\x61\x69\x73\x6d", 300);("\x62\x61\x62\x61", 238);("\x6b\x61\x6d\x61", 172)]);
(153, [("\x74\x65\x63\x68\x20\x6e\x65\x77\x73", 143);("\x74\x6f\x64\x61\x79", 121);("\x6f\x6e", 153);("\x6b\x69\x6c\x6f\x77\x61\x74\x74", 120);("\x77\x65", 140);("\x74\x61\x6c\x6b", 116);("\x61\x62\x6f\x75\x74", 124);("\x61\x6e\x64", 251);("\x74\x68\x65", 204);("\x6d\x6f\x64\x65\x6c", 163);("\x74\x65\x73\x6c\x61", 131)]);
(154, [("\x73\x70\x6f\x72\x74\x73\x20\x26\x20\x72\x65\x63\x72\x65\x61\x74\x69\x6f\x6e", 850);("\x6a\x6f\x73\x68", 107);("\x26", 281);("\x61\x72\x65", 475);("\x6a\x6f\x69\x6e\x65\x64", 144);("\x62\x79", 319);("\x61\x6e\x64", 4884);("\x66\x6f\x72", 1441);("\x74\x68\x69\x73", 1200);("\x65\x64\x69\x74\x69\x6f\x6e", 148);("\x6f\x66", 2923);("\x74\x68\x65", 10363);("\x73\x65\x61\x73\x6f\x6e", 401);("\x73\x6f", 253);("\x77\x69\x74\x68", 1590);("\x61", 2355);("\x6b\x6e\x69\x63\x6b\x73", 161);("\x77\x65\x65\x6b", 237);("\x6a\x61\x6b\x65", 156);("\x73\x70\x6f\x72\x74\x73", 166);("\x6a\x6f\x69\x6e\x73", 108)]);
(155, [("\x74\x68\x65", 1173);("\x6f\x66", 564);("\x69\x6e", 327);("\x74\x68\x69\x73", 269);("\x65\x70\x69\x73\x6f\x64\x65", 334);("\x77\x69\x74\x68", 227);("\x61\x6e\x64", 865);("\x61", 611);("\x74\x6f", 581);("\x61\x62\x6f\x75\x74", 224);("\x77\x65", 250);("\x74\x68\x61\x74", 123);("\x6f\x6e", 336);("\x6f\x75\x72", 104);("\x69\x73", 238);("\x79\x6f\x75", 165);("\x69\x74", 117);("\x68\x69\x73", 116);("\x61\x74", 106);("\x66\x6f\x72", 181);("\x75\x73", 104)]);
(156, [("\x61\x72\x74\x73", 296);("\x62\x6f\x6f\x6b", 544);("\x6f\x66", 11662);("\x74\x68\x65", 18718);("\x69\x73", 4375);("\x61\x62\x6f\x75\x74", 2433);("\x68\x6f\x77", 1455);("\x61\x6e\x64", 15358);("\x77\x65", 3016);("\x69\x6e\x74\x6f", 721);("\x65\x76\x65\x72\x79\x74\x68\x69\x6e\x67", 102);("\x6c\x69\x76\x65", 342);("\x69\x6e", 8149);("\x62\x65\x63\x61\x75\x73\x65", 213);("\x74\x6f", 9507);("\x74\x61\x6b\x65", 348);("\x69\x74", 1578);("\x66\x6f\x72", 4305);("\x77\x68\x65\x6e", 547);("\x79\x6f\x75", 4128);("\x73\x6f\x6d\x65\x74\x68\x69\x6e\x67", 224)]);
(157, [("\x75\x73", 123);("\x66\x6f\x72", 374);("\x6f\x75\x72", 179);("\x65\x70\x69\x73\x6f\x64\x65", 370);("\x6f\x66", 1152);("\x74\x68\x65", 2400);("\x73\x68\x6f\x77", 119);("\x77\x69\x74\x68", 400);("\x73\x6f\x72\x63\x65\x72\x65\x73\x73", 136);("\x6f\x6e", 422);("\x66\x72\x6f\x6d", 135);("\x74\x6f", 945);("\x69\x6e", 652);("\x77\x65", 279);("\x61", 686);("\x61\x6e\x64", 1033);("\x74\x68\x61\x74", 301);("\x68\x6f\x77", 146);("\x61\x72\x65", 152);("\x62\x79", 103);("\x69\x73", 338)]);
(158, [("\x6d\x75\x73\x69\x63", 156);("\x74\x68\x65", 198);("\x2d", 853);("\x28\x6f\x72\x69\x67\x69\x6e\x61\x6c", 169);("\x26", 134);("\x61", 252);("\x72\x61\x64\x69\x6f", 135);("\x64\x6a", 193);("\x6c\x61", 579);("\x6f\x6e", 105);("\x69\x74\x75\x6e\x65\x73\x3a", 120);("\x64\x65", 1398);("\x6c\x6f\x73", 185);("\x65\x6c", 302);("\x72\x65\x63\x6f\x72\x64", 194);("\x70\x6f\x64\x63\x61\x73\x74", 154);("\x70\x61\x72\x61", 116);("\x79", 613);("\x6c\x61\x73", 159);("\x64\x65\x6c", 185);("\x71\x75\x65", 232)]);
(1382, [("\x61\x6d\x61\x74\x65\x75\x72", 125);("\xe4\xb8\x8a\xe4\xb8\x80\xe4\xb8\x96\xef\xbc\x8c\xe8\x8b\x8d\xe5\x9b\xbd\xe4\xb8\x9e\xe7\x9b\xb8\xe4\xb9\x8b\xe5\xa5\xb3\xe4\xb8\x8a\xe5\xae\x98\xe6\xb2\xab\xef\xbc\x8c\xe7\xbb\x9d\xe7\xbe\x8e\xe5\x80\xbe\xe5\x9f\x8e\xef\xbc\x8c\xe6\x80\xa7\xe5\xad\x90\xe5\x8d\xb4\xe5\xa4\xaa\xe8\xbf\x87\xe8\xbd\xaf\xe5\xbc\xb1\xef\xbc\x8c\xe8\xa2\xab\xe5\xad\xaa\xe7\x94\x9f\xe5\xa6\xb9\xe5\xa6\xb9\xe6\x8a\xa2\xe5\xb0\xbd\xe4\xba\x86\xe6\x89\x80\xe6\x9c\x89\xe9\xa3\x8e\xe5\xa4\xb4\xef\xbc\x8c\xe5\x8d\xb4\xe5\x9c\xa8\xe9\x93\xb6\xe6\x9c\x88\xe5\x9b\xbd\xe8\xa6\x81\xe6\xb1\x82\xe8\x81\x94\xe5\xa7\xbb\xe4\xb9\x8b\xe6\x97\xb6\xef\xbc\x8c\xe8\xa2\xab\xe4\xba\xb2\xe4\xba\xba\xe6\xaf\xab\xe4\xb8\x8d\xe7\x95\x99\xe6\x83\x85\xe5\x9c\xb0\xe6\x8e\xa8\xe4\xba\x86\xe5\x87\xba\xe5\x8e\xbb\xef\xbc\x8c\xe5\x8f\xaa\xe4\xb8\xba\xe4\xbf\x9d\xe4\xbd\x8f\xe5\xa5\xb9\xe9\x82\xa3\xe4\xb8\xaa\xe5\xa6\x82\xe7\x8f\xa0\xe5\xa6\x82\xe5\xae\x9d\xe7\x9a\x84\xe5\xa6\xb9\xe5\xa6\xb9\xe3\x80\x82", 125);("\xe8\xbf\x99\xe4\xb8\x80\xe4\xb8\x96\xef\xbc\x8c\xe4\xba\x91\xe9\x97\xa8\xe5\xa4\xa7\xe5\xb0\x8f\xe5\xa7\x90\xe4\xba\x91\xe9\xa3\x8e\xe8\xbd\xbb\xef\xbc\x8c\xe8\xbf\x98\xe6\x9c\x89\xe4\xb8\x80\xe4\xb8\xaa\xe8\xba\xab\xe4\xbb\xbd\xef\xbc\x8c\xe4\xb8\x8a\xe5\xae\x98\xe5\xae\xb6\xe5\xae\xb6\xe4\xb8\xbb\xe4\xb8\x8a\xe5\xae\x98\xe6\xb2\xab\xef\xbc\x8c\xe5\x90\x8c\xe6\xa0\xb7\xe4\xb8\x8d\xe5\x8f\x97\xe5\xae\xb6\xe4\xba\xba\xe5\x96\x9c\xe7\x88\xb1\xe3\x80\x82", 125);("\xe5\x9b\xa0\xe4\xb8\xba\xe5\xb0\x8f\xe9\xac\xbc\xe5\x8b\xbe\xe9\x94\x99\xe4\xba\x86\x2e\x2e\x2e", 125);("\xe9\xac\xbc\xe7\x8e\x8b\xe5\xa6\x96\xe5\xa6\x83", 125);("\x2d\x2d", 125)]);
(1383, [("\x63\x68\x72\x69\x73\x74\x69\x61\x6e\x69\x74\x79", 819);("\x2d", 124);("\x61\x6e\x64", 102);("\x61", 101);("\x74\x68\x65", 206);("\x6f\x66", 108);("\x64\x65\x72", 102);("\x64\x65", 142)]);
(1384, [("\x6d\x61\x6e\x61\x67\x65\x6d\x65\x6e\x74\x20\x26\x20\x6d\x61\x72\x6b\x65\x74\x69\x6e\x67", 103);("\x69\x73\x74", 165);("\x65\x73", 109);("\x64\x65\x72", 401);("\x73\x69\x63\x68", 108);("\x66\xc3\xbc\x72", 299);("\x68\x69\x65\x72", 159);("\x64\x75", 459);("\x64\x69\x65\x73\x65", 136);("\x6e\x69\x63\x68\x74", 117);("\x61\x75\x66", 700);("\x7a\x65\x69\x74", 117);("\x64\x69\x65", 586);("\x75\x6d", 362);("\x64\x61\x73", 217);("\x75\x6e\x64", 720);("\x2d", 296);("\x6d\x65\x68\x72", 250);("\x77\x61\x73", 112);("\x64\x69\x63\x68", 109);("\x69\x6e", 288)]);
(1385, [("\x61", 629);("\x74\x6f", 776);("\x74\x68\x65", 1470);("\x61\x6e\x64", 1241);("\x74\x68\x61\x74", 225);("\x69\x6e", 548);("\x6f\x66", 963);("\x66\x72\x6f\x6d", 198);("\x74\x68\x69\x73", 266);("\x61\x74", 181);("\x69\x73", 232);("\x61\x6c\x73\x6f", 130);("\x63\x61\x6e", 125);("\x6f\x6e", 411);("\x62\x65", 121);("\x61\x62\x6f\x75\x74", 242);("\x65\x70\x69\x73\x6f\x64\x65", 115);("\x6e\x65\x77", 105);("\x66\x6f\x72", 331);("\x77\x69\x74\x68", 287);("\x77\x65", 317)]);
(1386, [("\x73\x65\x78\x75\x61\x6c\x69\x74\x79", 299);("\x74\x68\x69\x73", 8509);("\x77\x65\x65\x6b", 1783);("\x69\x73", 11638);("\x6a\x6f\x69\x6e\x65\x64", 907);("\x62\x79", 5867);("\x61\x6e\x64", 40973);("\x74\x6f", 32138);("\x64\x69\x73\x63\x75\x73\x73", 3011);("\x64\x75\x72\x69\x6e\x67", 415);("\x74\x68\x65", 47602);("\x74\x69\x6d\x65\x73", 202);("\x74\x68\x65\x79", 2373);("\x6a\x6f\x68\x6e", 252);("\x61", 20911);("\x74\x69\x70\x73", 390);("\x66\x6f\x72", 11548);("\x66\x75\x6c\x6c", 642);("\x62\x6f\x64\x79", 215);("\x61\x6c\x73\x6f", 1670);("\x67\x75\x79\x73", 438)]);
(1388, [("\x61", 116);("\x61\x6e\x64", 176);("\x74\x68\x65", 229);("\x6f\x66", 168);("\x69\x6e", 128)]);
(1390, [("\x74\x68\x65", 127)]);

]

let test_sums = [(0, 705623); (1, 1321); (2, 129561); (3, 392); (4, 47159); (5, 926708); (6, 73177); (7, 2202332); (8, 33018); (9, 2312); (10, 89386); (11, 21); (12, 108); (13, 15006); (14, 612); (15, 326067); (16, 10711); (17, 7680); (18, 12617); (19, 24666); (20, 91488); (21, 67381); (22, 87777); (23, 529); (24, 39184); (25, 889444); (26, 23638); (27, 1021532); (28, 1510); (29, 12101); (30, 20961); (31, 8578); (32, 1509); (33, 629); (34, 38939); (35, 127); (36, 5850); (37, 3125); (38, 12910); (39, 17945); (40, 45189); (41, 1959); (42, 6721); (43, 7723); (44, 2489); (45, 11609); (46, 883431); (47, 116486); (48, 3951997); (49, 568776); (50, 4884); (51, 1606); (52, 37133); (53, 17438); (54, 5812); (55, 751943); (56, 9716); (57, 2762318); (58, 184930); (59, 3849845); (60, 15); (61, 8309); (62, 2421); (63, 392); (64, 313417); (65, 35599); (66, 9093); (67, 10024); (68, 128859); (69, 1463); (70, 402937); (71, 9402); (72, 8789); (73, 247597); (74, 2576); (75, 5498); (76, 1392); (77, 7559); (78, 53815); (79, 619); (80, 3201); (81, 1351); (82, 37890); (83, 4735); (84, 5109); (85, 37933); (86, 32331); (87, 22940); (88, 222696); (89, 18173); (90, 24263); (91, 1563); (92, 46610); (93, 1872); (94, 2545); (95, 95329); (96, 67400); (97, 115585); (98, 3854); (99, 462374)]

let test_bin_fname = 
  let cwd = Sys.getcwd () in 
  let fname = Printf.sprintf "%s/token_dists.bin" cwd in 
  Re.replace_string (Re.Posix.compile_pat "_build/default/lib/histogram/test") ~by:"lib/histogram" fname

let test_hists = Histogram.load test_bin_fname

let test_test_counts _ =
  for _ = 0 to 200000 do
  test_counts
  |> List.iter (fun pair ->
    let idx, vals = pair in
    let hist = Array.get test_hists idx in 
    vals
    |> List.iter (fun v ->
      let k, c = v in 
      let hc = Histogram.get hist k in 
      assert_equal ~msg:"count is correct" c hc;
    )
  )
  done

let test_test_sums _ =
  for _ = 0 to 100000 do
      test_sums
      |> List.iter (fun pair ->
        let idx, c = pair in 
        let hist = Array.get test_hists idx in 
        let hc = Histogram.sum hist in
        assert_equal ~msg:"sum is correct" c hc;
      )
  done

let test_length _ =
  assert_equal (Array.length test_hists) 1392;
  assert_equal (Histogram.length (Array.get test_hists 0)) 65763;
  assert_equal (Histogram.length (Array.get test_hists 1)) 546;
  assert_equal (Histogram.length (Array.get test_hists 2)) 16927;
  assert_equal (Histogram.length (Array.get test_hists 18)) 4125

let suite = 
  "suite">::: [
    "counts">:: test_test_counts;
    "sums">:: test_test_sums;
    "lengths">:: test_length
  ]

let () =
  run_test_tt_main suite