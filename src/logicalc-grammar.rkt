#lang brag
@WFF:EQV|IMP|OR|AND|NOT|ID
@ID:VAL|/'(' WFF /')'|NOT|VAR
EQV:WFF (/'=' IMP)*
IMP:WFF (/'>' OR)*
OR:WFF (/'^' AND)*
AND:WFF (/'*' ID)*
NOT:/'~' ID
VAL:"1" | "0"
VAR:("a"|"b"|"c"|"d"|"e"|"f"|"g"|"h"|"i"|"j"|"k"|"l"|"m"|"n"|"o"|"p"|"q"|"r"|"s"|"t"|"u"|"v"|"w"|"x"|"y"|"z"
     "A"|"B"|"C"|"D"|"E"|"F"|"G"|"H"|"I"|"J"|"K"|"L"|"M"|"N"|"O"|"P"|"Q"|"R"|"S"|"T"|"U"|"V"|"W"|"X"|"Y"|"Z")+