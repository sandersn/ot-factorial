from util.reflect import traced
from itertools import repeat
from util.lst import transpose, partition, fst, snd
def extractions(l):
    acc = []
    for i in range(len(l)):
        acc.append((l[i], l[:i]+l[i+1:]))
    return acc
def alwaysZip(first, second):
    return zip(first, repeat(second) if second==[] else second)
def filterProxy(f, p, l):
    return [x for d,x in zip(p,l) if f(d)]
L = 'L'
W = 'W'
E = 'e'
@traced
def rVolume(cols):
    def r(col,cols):
        _,erc = col
        if any(v==L for v in erc): return False
        if all(v==W for v in erc): return True
        else: return rVolume(rem(cols, col))
    return any(r(col,cols) for col,cols in extractions(cols))
def rem(cols, (title, erc)):
    titles, ercs = transpose(cols)
    filtered = filterProxy(lambda v: v!=W, erc, transpose(ercs))
    return alwaysZip(titles, transpose(filtered))
@traced
def rcd(cols):
    demote,promote = partition(lambda (_,vlns): any(v==L for v in vlns), cols)
    if promote==[]: return False
    if demote==[]: return True
    else: return rcd(rem2(demote, promote))
def rem2(demote, promote):
    titles = map(fst, demote)
    vlns = transpose(map(snd, demote))
    filtered = filterProxy(lambda x: all(y==E for y in x), transpose(map(snd, promote)), vlns)
    return alwaysZip(titles, transpose(filtered))


cost = [[("*Complex",[L,L,L,E,L]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[W,E,W,E,W])],
        [("*Complex",[L,L,L,E,W]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[W,E,W,E,L])]
,[("*Complex",[L,L,W,W,L]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[W,E,L,L,W])]
,[("*Complex",[L,L,W,W,W]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[W,E,L,L,L])]

,[("*Complex",[L,L,L,E,L]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,L,L,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[W,E,W,E,W])]

,[("*Complex",[L,L,L,E,W]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,L,L,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[W,E,W,E,L])]

,[("*Complex",[W,E,L,E,L]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[L,L,W,E,W])]

,[("*Complex",[W,E,L,E,W]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[L,L,W,E,L])]

,[("*Complex",[W,E,W,W,L]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[L,L,L,L,W])]

,[("*Complex",[W,E,W,W,W]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,E,W,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[L,L,L,L,L])]

,[("*Complex",[W,E,L,E,L]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,L,L,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[L,L,W,E,W])]

,[("*Complex",[W,E,L,E,W]),("Onset",[E,L,E,E,E]),("Align-L-W",[E,W,L,L,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[L,L,W,E,L])]

,[("*Complex",[E,W,L,E,L]),("Onset",[W,W,E,E,E]),("Align-L-W",[L,L,E,W,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[W,E,W,E,W])]

,[("*Complex",[E,W,L,E,W]),("Onset",[W,W,E,E,E]),("Align-L-W",[L,L,E,W,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[W,E,W,E,L])]

,[("*Complex",[E,W,W,W,L]),("Onset",[W,W,E,E,E]),("Align-L-W",[L,L,E,W,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[W,E,L,L,W])]
,[("*Complex",[E,W,W,W,W]),("Onset",[W,W,E,E,E]),("Align-L-W",[L,L,E,W,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[W,E,L,L,L])]
,[("*Complex",[E,W,L,E,L]),("Onset",[W,W,E,E,E]),("Align-L-W",[L,L,L,L,E]),("Align-R-P",[E,E,E,E,W]),("Parse",[W,E,W,E,W])]
,[("*Complex",[E,W,L,E,W]),("Onset",[W,W,E,E,E]),("Align-L-W",[L,L,L,L,E]),("Align-R-P",[E,E,E,E,L]),("Parse",[W,E,W,E,L])]
]
