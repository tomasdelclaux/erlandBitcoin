# erlandBitcoin
Hashing in erland for bitcoins

## INTRODUCTION
Distributed system for mining bitcoins in erlang.

## EXAMPLE 1
If running the program file on a single, make sure that the DOS flag is set to false.
```
-define(DOS, true).
```
In order to run example 1. Compile the file bitcoinWorker and run the function example1().
```
MacBook-Pro erlandBitcoin % erl
Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:10:10] [ds:10:10:10] [async-threads:1] [jit] [dtrace]

Eshell V13.0.4  (abort with ^G)
1> c(bitcoinWorker).
bitcoinWorker.erl:2:2: Warning: export_all flag enabled - all functions will be exported
%    2| -compile(export_all).
%     |  ^

{ok,bitcoinWorker}
2> bitcoinWorker:example1().
4
tomas.delclauxro;Iw3oj+ARSFP9wFXDBXPgVW7mrbprWzrj       0000e773f4f16b6ba86c7b71d0d2a74b32fb0c4256f93bac6e1e145f19051286
ok
```

## EXAMPLE 2
In order to run the system in distributed machines (ie: separate machines across an ip network), the following steps must be done:

#### Put a static dns entry on both machines.Start each erlang vm with the '-name' flag set to the hostname running the process
On machine number 1 (names and ip addresses need to be adjusted accordingly)
```
echo "192.168.64.6    hostname2.com" >> /etc/hosts
erl -name node@hostname1.com
```

On machine number 2 (names and ip addresses need to be adjusted accordingly)
```
echo "192.168.64.1 hostname1.com" >> /etc/hosts
erl -name node@hostname2.com
```

On the bitcoinServer and the bitcoinWorker file, set the DOS flag to true
```-define(DOS, true).```

Then on the client machine run the client program and on the server machine run the server program.

Client side:
```
(node@hostname2.com)2> bitcoinWorker:example2().
192.168.64.1
```

Server Side:
```
(node@hostname1.com)2> bitcoinServer:start().
4500
SERVER:<0.100.0> tomas.delclauxro;SkByh631ZqwzrThMt+K3lH/Yq5A7XjFA      0000095e52ec7b3900bd7a107b63456136144d373e994ed60d25e4807ba552f7
SERVER:<0.96.0> tomas.delclauxro;rKLcnpBGeIocCx7ADNW+7ToleAyAIk4n       0000085c15162f687a08d4ecab6fe8c44152a7b1888acf7ccd0239d253d96715
SERVER:<0.108.0> tomas.delclauxro;IxGohYToTKzRRpriwf1MIKQz/G13bfVb      000004ad6ac529f7d0f7886dbe891b4c07b0fd9535869fe09f70f16ec4889ddc
SERVER:<0.104.0> tomas.delclauxro;vV1S0JBRDmO9WTST3jJXuS4ly+OBP2Cn      000002207f5d7ce64830c5ac2bc7cf74f2fc04f5a9ba1063384065ebb7251210
SERVER:<0.100.0> tomas.delclauxro;djJN7NXvC2r4EhctISZiZl9TQr877oBg      00000bd9fc96133f319a8750ee5846ca6b8f9da2e51585f86bd7ebfc86e28268
SERVER:<0.102.0> tomas.delclauxro;13W8iZbCFC/WGS1yJn6HvAGqcjWWWx/y      0000038325075b8692408003dd2e2c1633e35fef0177920520fe01678a6f44bc
SERVER:<0.94.0> tomas.delclauxro;WbV5BVvOopLEHDOuHkIAaJVKgJ4vldJu       000003e9eacbd6e971d7b911e3f9f6c973c662a199fb83bd292fe45e9e985cc4
SERVER:<0.100.0> tomas.delclauxro;UWTqhyECccUfh/RaqXZbr+dNDE+HiZvl      00000024c360fa14f2e6174667deee896ab4dd4ea07d2db0c96125de2b08a0d6
SERVER:<0.94.0> tomas.delclauxro;mHRhQsCqBHgUOZsfu5s97U0lP/TCG0lt       0000050db35896c5bfffdc29758d651978896d58daeedeb6e41f1fa9470fd6f2
SERVER:<0.108.0> tomas.delclauxro;FNSFrBEuEgsqpx51sM14NedAB0Qkk9P2      00000cb79d0084f44bd7f2b33a148d1f958ccbbc7a43e5c976786cfdf23e868d
WORKER:<0.86.0> tomas.delclauxro;Ms/OP5ctZyP3omFHzT3vd0/GbVg3OCIP       000004efa3df7f587d1c4e225bec825b79d47c55b7a9b3d5845877d2384e8d87
```

To stop the server run:

```
(node@hostname1.com)3> bitcoinServer:stop().
```

## ACTOR MODEL

###SIZE OF WORK UNIT - TODO

###RESULT OF RUNNING FOR INPUT 4 - TODO

###CPU TIME VS ELAPSED TIME - HOW MUCH PARALLELISM - TODO
```
(node@hostname1.com)3> bitcoinServer:stop().
Cpu time=193195000 microseconds
Elapsed time=147463000 microseconds
** exception exit: killed
``` 
### LARGEST COIN FOUND - TODO

### LARGEST NUMBER OF WORKING MACHINES
