import sys
import copy
import intervals as I
from collections import OrderedDict

nbpiece = 10

class Compte :
    def __init__(self, piece, niveau = 0) :
        self.trans = dict()
        self.niveau = niveau
        self.piece = piece
        self.defense = set()

def ico(x, y) :
    return I.closedopen(x, y)

lc = dict()
lc[ico(0, nbpiece)] = Compte({ico(0, nbpiece)})
lc[ico(nbpiece, I.inf) | ico(-I.inf, 0)] = Compte(set())
global comptes_actifs, comptes_morts
comptes_actifs = ico(0, nbpiece)
comptes_morts = I.empty()

def sousinter(K, k, V) :
    if V == I.empty() or K == I.empty() or k == I.empty() :
        return(I.empty())
    offset = V.lower - K.lower
    v = I.empty()
    for i in list(k) :
       	v |= ico(i.lower + offset, i.upper + offset)
    return(v)

def adapte_coupe(K, k, compte) :
    newc = Compte(set())
    for cle in compte.trans :
        newc.trans[sousinter(K, k, cle[0]), cle[1]] = compte.trans[cle]
    for piece in compte.piece :
        newc.piece.add(sousinter(K, k, piece))
    for (sender, receiver, nxtlvl) in compte.defense :
        sender2 = sousinter(K, k, sender)
        receiver2 = sousinter(K, k, receiver)
        newc.defense.add((sender2, receiver2, nxtlvl))
    lc[k] = newc

def check(k, v) :
    if k == I.empty() or (k in lc and v in lc) :
        return
    for (cle, compte) in lc.items() :
        kcommun = cle & k
        if kcommun != I.empty() :
            vcommun = sousinter(k, kcommun, v)
            k -= kcommun
            v -= vcommun
            if kcommun != cle :
                adapte_coupe(cle, kcommun, compte)
                adapte_coupe(cle, cle - kcommun, compte)
                lc.pop(cle)
            check(vcommun, kcommun)
            check(k, v)
            break
    check(v, k)

def find_parts(k) :
    return [cle for cle in lc if k.contains(cle)]

def sizeunion(union) :
    if union == I.empty() :
        return 0
    l = list(union)
    s = 0
    for i in l :
        s+= i.upper - i.lower
    return s

def update(sender, ((receiver, nxtlvl), votants)) :
    global comptes_actifs
    global comptes_morts
    if lc[sender].niveau > nxtlvl :
        return
    union = I.empty()
    for part in find_parts(votants):
        if lc[part].niveau == lc[sender].niveau :
            for piece in lc[part].piece :
                union |= piece
    if sizeunion(union) < .75 * nbpiece:
        return
    if not lc[receiver].piece :
        comptes_actifs |= receiver
        comptes_morts |= sender
    comptes_actifs -= sender
    lc[receiver].piece = lc[receiver].piece.union(lc[sender].piece)
    if lc[receiver].niveau == 0 :
        lc[receiver].niveau = nxtlvl
    else :
        lc[receiver].niveau = min(lc[receiver].niveau, nxtlvl)
    for triplet in lc[receiver].defense :
        recep(receiver, triplet)
    for (receiver2, nxtlvl2) in lc[receiver].trans :
        recep(I.empty(), (receiver, receiver2, nxtlvl2))

def recep_aux(l) :
    [compte, sender, receiver, nxtlvl] = l
    (vs, rb) = (lc[sender].trans, (receiver, nxtlvl))
    if rb not in vs :
        lc[compte].defense.add((sender, receiver, nxtlvl))
    vs[rb] = vs.get(rb, I.empty()) | compte
    if lc[sender].piece :
        update(sender, (rb, vs[rb]))

def recep(compte, (sender, receiver, nxtlvl)) :
    if sizeunion(sender) != sizeunion(receiver) :
        receiver = sousinter(sender, sender, receiver)
        print("taille receiver corrigee", receiver)
    sender -= sender & receiver
    receiver -= sender & receiver
    check(compte, I.empty())
    check(sender, receiver)
    f = find_parts(compte)
    if not f :
        f = [I.empty()]
    for comptei in f:
        for senderj in find_parts(sender) :
            receiverj = sousinter(sender, senderj, receiver)
            for receiverk in find_parts(receiverj) :
                #debug : print("compte", comptei, "sender", senderj, "receiver", receiverk, "parts", lc.keys())
                recep_aux([comptei, senderj, receiverk, nxtlvl])

print("dead :", comptes_morts)
print("position :", comptes_actifs)
while(1) :
    line = sys.stdin.readline().rstrip().split(" ")
    if len(line) == 7 :
        [cl, cu, sl, su, rl, ru, nxtlvl] = map(float,(line))
        recep(ico(min(cl, cu), max(cl, cu)), (ico(min(sl, su), max(sl, su)), ico(min(rl, ru), max(rl, ru)), nxtlvl))
    else :
        print("ligne invalide")
    print("dead :", comptes_morts)
    print("position :", comptes_actifs)
