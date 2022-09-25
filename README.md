# erlangBitcoin
Erlang simulation of mining bitcoins in a distributed system.
The project consists of a parallel processes mining server that can also accept clients to participate in the mining process.

# AUTHORS
Ariel Weitzenfeld and Tomas Delclaux

## INTRODUCTION
Distributed system for mining bitcoins in erlang.


## EXAMPLE 1
If running the program file on a single, make sure that the DOS flag is set to false.
```
-define(DOS, false).
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

Client side (with an imput for the IP of the server):
```
(node@hostname2.com)2> bitcoinWorker:example2().
192.168.64.1
```
Additionally, the bitcoinWorker or client can also be run with the following command and specifiying the ip address:
```
bitcoinWorker:client("127.0.0.1")
```

Server Side (with an input for a number of zeros):
```
(node@hostname1.com)2> bitcoinServer:start().
5
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

SIZE OF WORK UNIT -

The optimal number of workers we decided is around 8. This is because our machine has 10 cores, but two of those are energy efficient cores.

Each worker gets the same size of work unit as each worker operates on a first come first serve basis. It requests work, looks for a coin, reports after finding one, and requests work again. The number of times a worker is able to run a problem is dependent on the scheduler and the randomness of successful operations.

Additionally, the hashing operation is atomic and cannot be divided among multiple actors for faster results. Instead, the idea is to maximise the number of actors or workers to increase the probability of finding a coin faster.

Hence, we decided to have workers mine coins with a specific numbers as determined by the input to the server. Also, some actors were used on the server side to be able to accommodate more than one tcp client at a time. Since, the tcp connection is only needed to get the information from the server on how many leading zeroes a coin must have, and to then report back the results, these number of tcp processes were limited to 2. The reason for this, is that we set the number of leading zeroes to be above 5, which reduces the rate at which coins are found, and hence there is more need to have more processes mining than servers processing tcp client connections simultaneously.

RESULT OF RUNNING FOR INPUT 4 -
```
4
4500
WORKER:<0.79.0> tomas.delclauxro;swJ6jyWs599YpUi9FFjfhIPykn9DDFS+       00009d846cc30176c3ff4f75d090953249e4732f52b436d3322b855f89de74f8
WORKER:<0.79.0> tomas.delclauxro;1rccY65PRu99pXR686eNLdMoH4rYc5tj       0000a91e5f90651f0bf4b263976974794eb53c23aff730d4002d70f53bba75bb
WORKER:<0.79.0> tomas.delclauxro;HsYWwpureyTprfr6sY4HVpZrC3E+hRo2       00007921ee51f522edb042dd0bf3d0978b1c12cc4c9d45293ca9c8818310780c
WORKER:<0.79.0> tomas.delclauxro;ZaSlIPKTW73AP0li/IY+nPaaa9CjPJXL       00003fbc9b5a20b76e01d22258a0c8f25c555da2055453991d26b48ba2c50e6e
SERVER:<0.92.0> tomas.delclauxro;YP376HeK7t1KjZH6Dzmu036HfaF/GRCJ       0000fadaab7b91abdbea423c722249297223ff546c38d09efbf09e78ec629838
SERVER:<0.95.0> tomas.delclauxro;9I+TH8M6YK3Ut7jCCb3QsfCAF85HenPp       00004b99561cb001b6267558f973e289990f15eea8420dc2ad96035eaa90c298
SERVER:<0.98.0> tomas.delclauxro;gnHPcRbqyV/7QXnErr3hFHHxzoqjC0iz       00003b504095c50cb5cbc6744899a33d905424488db2ad68fa5213e05d8b76bc
WORKER:<0.79.0> tomas.delclauxro;9MfgBFqgl+H0xpqcxKuwecteDrikB/zf       00002cc066f3cbebd2fc925bbc5d5726704769cc83e6e1689703f5efa1382054
```

CPU TIME VS ELAPSED TIME - HOW MUCH PARALLELISM -
```
(node@hostname1.com)3> bitcoinServer:stop().
Cpu time=747802000 microseconds
Elapsed time=236173000 microseconds
** exception exit: killed
``` 
CPU to Realtime Ratio ~= 3.16633146041

### LARGEST COIN FOUND - 
7 Zeros
```
tomas.delclauxro;GvSJdEEvPl37E5Rwiv8bHrF0nI/r/EJj      000000063a9e9a68100adc9980abf79f2dd331a2ea2de855f9b28cd0c6989234
```

### LARGEST NUMBER OF WORKING MACHINES

The largest number of independent machines we were able to run the code with was two separate machines. The reason for this is due to the technical limitation of having many physical machines to run the code with.

We were, however, able to run the code with 4 connected clients (these were client connections from a virtual machine), and the server running on the host.

Without any clients connecting to the server, the maximum number of feasible actors to use to run bitcoin mining processes on the same machine as the 
server was determined by the number of physical cores of the system. Both our systems had 8 high performance cores and 2 efficiency cores. Consequently, the maximum number of processes running in parallel could at max be 10. However, due to the erlang scheduler and the erlang vm, the code can run with much more actors. In fact, we tested running the code with up to 50 bitcoin mine workers on the server machine.
