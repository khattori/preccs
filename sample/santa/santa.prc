C{
#include <stdlib.h>
#include <stdio.h>
C}
type Gate = { in:<unit>; out:<unit>; bell:<unit>; n:int }
proc CreateGate(n:int):Gate =
    var in:<unit>; var out:<unit>; var bell:<unit>;
    var gate = { in=in; out=out; bell=bell; n=n };
    return gate
proc Main() = 
    var reindeer_gate = CreateGate(9);
    var elf_gate = CreateGate(3);
    GateKeeper(reindeer_gate);
    GateKeeper(elf_gate);
    Santa(reindeer_gate, elf_gate);
    CreateWorkers(9,reindeer_gate,"Reindeer","deliver toys");
    CreateWorkers(10,elf_gate,"Elf","meet in my study")
proc Santa(reindeer_gate:Gate,elf_gate:Gate) =
  { reindeer_gate.bell?x ->
       C{ printf("Ho! Ho! Ho! let's deliver toys\n"); C};
       OpenGate(reindeer_gate)
  | elf_gate.bell?x ->
       C{ printf("Ho! Ho! Ho! let's meet in my study\n"); C};
       OpenGate(elf_gate)
  }; Santa(reindeer_gate, elf_gate)
proc OpenGate(gate:Gate) = OG_Loop(gate.n, gate.out)
proc OG_Loop(i:int,ogate:<unit>) =
    i @ 0 -> stop
      | _ -> ogate!(); OG_Loop(i-1,ogate)
proc GateKeeper(gate:Gate) = GK_Loop(gate.n,gate)
proc GK_Loop(i:int,gate:Gate) =
    i @ 0 -> gate.bell!(); GateKeeper(gate)
      | _ -> gate.in?x; GK_Loop(i-1,gate)
proc CreateWorkers(n:int,gate:Gate,kind:string,job:string) =
    n @ 0 -> stop
      | _ -> Worker(n,gate,kind,job);
             CreateWorkers(n-1,gate,kind,job)
proc Worker(id:int,gate:Gate,kind:string,job:string) =
  RandomSleep(1000)!();
  gate.in!();
  gate.out?x;
  C{ printf("%s: %d %s\n", STRPTR($kind$), TOCINT($id$), STRPTR($job$)); C};
  Worker(id, gate, kind, job)
proc RandomSleep(n:int):<unit> = 
   var fire:<unit>;
   return fire;
   var rand:int;
   C{  int r = rand(); $rand$ = r % TOCINT($n$); C};
   timer!(1,rand);
   fire?x
