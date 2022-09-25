# erlangBitcoin
Erlang simulation of mining bitcoins in a distributed system.
The project consists of a parallel processes mining server that can also accept clients to participate in the mining process.

# AUTHORS
Ariel Weitzenfeld and Tomas Delclaux

## INTRODUCTION
Distributed system for mining bitcoins in erlang.
The file bitcoinServer.el contains the code for the server and bitcoinWorker.erl contains the file for the client.

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

SIZE OF WORK UNIT 

The optimal number of workers we decided is around 8. This is because our machine has 10 cores, but two of those are energy efficient cores.

Each worker gets the same size of work unit as each worker operates on a first come first serve basis. It requests work, looks for a coin, reports after finding one, and requests work again. The number of times a worker is able to run a problem is dependent on the scheduler and the randomness of successful operations.

Additionally, the hashing operation is atomic and cannot be divided among multiple actors for faster results. Instead, the idea is to maximise the number of actors or workers to increase the probability of finding a coin faster.

Hence, we decided to have workers mine coins with a specific numbers as determined by the input to the server. Also, some actors were used on the server side to be able to accommodate more than one tcp client at a time. Since, the tcp connection is only needed to get the information from the server on how many leading zeroes a coin must have, and to then report back the results, these number of tcp processes were limited to 2. The reason for this, is that we set the number of leading zeroes to be above 5, which reduces the rate at which coins are found, and hence there is more need to have more processes mining than servers processing tcp client connections simultaneously.

RESULT OF RUNNING FOR INPUT 4 
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

CPU TIME VS ELAPSED TIME 
```
Cpu time=79600000 microseconds
Elapsed time=20318000 microseconds
Ratio=3.917708435869672
``` 
CPU to Realtime Ratio ~= 3.9

This ratio was achived with the following actor configuration and entering 5 as the input for number of leading zeroes on the bitcoinServer:
```
-define(NUM_THREADS_SERVERS, 1).
-define(NUM_SERVER_WORKERS, 4).
```

### LARGEST COIN FOUND 
7 Zeros
```
tomas.delclauxro;GvSJdEEvPl37E5Rwiv8bHrF0nI/r/EJj      000000063a9e9a68100adc9980abf79f2dd331a2ea2de855f9b28cd0c6989234
```

### LARGEST NUMBER OF WORKING MACHINES

The largest number of independent machines we were able to run the code with was two separate machines. The reason for this is due to the technical limitation of having many physical machines to run the code with.

We were, however, able to run the code with 4 connected clients, and the server running on the host. In order to this, more actors had to spawn to take care of the tcp connections. So the following was set on the bitcoinServer file:
```
-define(NUM_THREADS_SERVERS, 4).
-define(NUM_SERVER_WORKERS, 4).
```
4 tcp threads to handle clients and 4 actors to mine on the server.

Without any clients connecting to the server, the maximum number of feasible actors to use to run bitcoin mining processes on the same machine as the 
server was determined by the number of physical cores of the system. Both our systems had 8 high performance cores and 2 efficiency cores. Consequently, the maximum number of processes running in parallel could at max be 10. However, due to the erlang scheduler and the erlang vm, the code can run with much more actors. In fact, we tested running the code with up to 50 bitcoin mine workers on the server machine.

```
87> regs().
SERVER:<0.495.0> tomas.delclauxro;leSlRt7rrOW9emt/2V9hG5vFvrxVOCvX      0000238b0275e4032fd54e9e671fa7a909c0afbd65a69d545333fb62f270042a

** Registered procs on node nonode@nohost **
Name                  Pid          Initial Call                      Reds Msgs
application_controlle <0.44.0>     erlang:apply/2                     907    0
bitcoin_server_miner1 <0.537.0>    bitcoinServer:super_find_     39016619    0
bitcoin_server_miner1 <0.528.0>    bitcoinServer:super_find_     33059516    0
bitcoin_server_miner1 <0.527.0>    bitcoinServer:super_find_     26602771    0
bitcoin_server_miner1 <0.526.0>    bitcoinServer:super_find_     26596445    0
bitcoin_server_miner1 <0.525.0>    bitcoinServer:super_find_     32032272    0
bitcoin_server_miner1 <0.524.0>    bitcoinServer:super_find_     31644449    0
bitcoin_server_miner1 <0.523.0>    bitcoinServer:super_find_     31632557    0
bitcoin_server_miner1 <0.522.0>    bitcoinServer:super_find_     31609894    0
SERVER:<0.516.0> tomas.delclauxro;xjeNJ1H9nwV0P9TuhC9DEMYP4VN9X7Vr      0000fe553f5abb283446dde5170f26ee643b698e0517c11fd88941670d19b9d6
bitcoin_server_miner1 <0.521.0>    bitcoinServer:super_find_     31640482    0
SERVER:<0.490.0> tomas.delclauxro;kepUu8zKxGz0O0eChgDPUFoyOx/Nac9I      00002441f313f18d41bef35999efb26f0c7a43857d3dc9d66818fa742cdc45e5
bitcoin_server_miner1 <0.520.0>    bitcoinServer:super_find_     26910515    0
bitcoin_server_miner1 <0.519.0>    bitcoinServer:super_find_     56726902    0
bitcoin_server_miner2 <0.536.0>    bitcoinServer:super_find_     28251639    0
bitcoin_server_miner2 <0.518.0>    bitcoinServer:super_find_     26725394    0
bitcoin_server_miner2 <0.517.0>    bitcoinServer:super_find_     31879101    0
bitcoin_server_miner2 <0.516.0>    bitcoinServer:super_find_     26966466    0
bitcoin_server_miner2 <0.515.0>    bitcoinServer:super_find_     31668404    0
bitcoin_server_miner2 <0.514.0>    bitcoinServer:super_find_     26730790    0
bitcoin_server_miner2 <0.513.0>    bitcoinServer:super_find_     26946492    0
bitcoin_server_miner2 <0.512.0>    bitcoinServer:super_find_     26951897    0
bitcoin_server_miner2 <0.511.0>    bitcoinServer:super_find_     31679054    0
bitcoin_server_miner2 <0.510.0>    bitcoinServer:super_find_     39206277    0
bitcoin_server_miner2 <0.509.0>    bitcoinServer:super_find_     26962434    0
bitcoin_server_miner3 <0.535.0>    bitcoinServer:super_find_     26682990    0
bitcoin_server_miner3 <0.508.0>    bitcoinServer:super_find_     26990879    0
bitcoin_server_miner3 <0.507.0>    bitcoinServer:super_find_     32275181    0
bitcoin_server_miner3 <0.506.0>    bitcoinServer:super_find_     39213866    0
bitcoin_server_miner3 <0.505.0>    bitcoinServer:super_find_     32646997    0
bitcoin_server_miner3 <0.504.0>    bitcoinServer:super_find_     36510930    0
bitcoin_server_miner3 <0.503.0>    bitcoinServer:super_find_     32937861    0
bitcoin_server_miner3 <0.502.0>    bitcoinServer:super_find_     39146402    0
bitcoin_server_miner3 <0.501.0>    bitcoinServer:super_find_     42797202    0
bitcoin_server_miner3 <0.500.0>    bitcoinServer:super_find_     27028133    0
bitcoin_server_miner3 <0.499.0>    bitcoinServer:super_find_     31193761    0
bitcoin_server_miner4 <0.534.0>    bitcoinServer:super_find_     27014332    0
bitcoin_server_miner4 <0.498.0>    bitcoinServer:super_find_     26730254    0
bitcoin_server_miner4 <0.497.0>    bitcoinServer:super_find_     33033383    0
bitcoin_server_miner4 <0.496.0>    bitcoinServer:super_find_     26838757    0
bitcoin_server_miner4 <0.495.0>    bitcoinServer:super_find_     69825896    0
bitcoin_server_miner4 <0.494.0>    bitcoinServer:super_find_     32039104    0
bitcoin_server_miner4 <0.493.0>    bitcoinServer:super_find_     59435155    0
bitcoin_server_miner4 <0.492.0>    bitcoinServer:super_find_     33461198    0
bitcoin_server_miner4 <0.491.0>    bitcoinServer:super_find_     55143602    0
bitcoin_server_miner4 <0.490.0>    bitcoinServer:super_find_     51858370    0
bitcoin_server_miner4 <0.489.0>    bitcoinServer:super_find_     51168672    0
bitcoin_server_miner5 <0.533.0>    bitcoinServer:super_find_     26770958    0
bitcoin_server_miner5 <0.488.0>    bitcoinServer:super_find_     39236227    0
bitcoin_server_miner6 <0.532.0>    bitcoinServer:super_find_     27065455    0
bitcoin_server_miner7 <0.531.0>    bitcoinServer:super_find_     26859397    0
bitcoin_server_miner8 <0.530.0>    bitcoinServer:super_find_     27054257    0
bitcoin_server_miner9 <0.529.0>    bitcoinServer:super_find_     39258311    0
bitcoin_server_tcp_1  <0.487.0>    bitcoinServer:server/2              28    0
bitcoin_server_tcp_2  <0.486.0>    bitcoinServer:server/2              64    0
code_server           <0.50.0>     erlang:apply/2                  159234    0
erl_prim_loader       <0.10.0>     erlang:apply/2                  285937    0
erl_signal_server     <0.59.0>     gen_event:init_it/6                145    0
erts_code_purger      <0.1.0>      erts_code_purger:start/0        153884    0
file_server_2         <0.58.0>     file_server:init/1                5161    0
global_group          <0.57.0>     global_group:init/1                192    0
global_name_server    <0.54.0>     global:init/1                      281    0
inet_db               <0.51.0>     inet_db:init/1                     569    0
init                  <0.0.0>      erl_init:start/2                  6115    0
kernel_refc           <0.67.0>     kernel_refc:init/1                 166    0
kernel_safe_sup       <0.68.0>     supervisor:kernel/1                191    0
kernel_sup            <0.49.0>     supervisor:kernel/1               2965    0
logger                <0.42.0>     logger_server:init/1               737    0
logger_handler_watche <0.70.0>     logger_handler_watcher:in          150    0
logger_proxy          <0.71.0>     logger_olp:init/1                  184    0
logger_std_h_default  <0.73.0>     logger_olp:init/1                  256    0
logger_sup            <0.69.0>     supervisor:logger_sup/1            508    0
rex                   <0.52.0>     rpc:init/1                         133    0
socket_registry       <0.7.0>      socket_registry:start/0            106    0
standard_error        <0.61.0>     erlang:apply/2                     112    0
SERVER:<0.512.0> tomas.delclauxro;A0ldh77OTLbTPjRfFQapGia5B0qVk54w      00001e7bcecf90076146ef63be55ebc2036957b8ccae7e9e69034e0e41c8045c
standard_error_sup    <0.60.0>     supervisor_bridge:standar          149    0
user                  <0.64.0>     group:server/3                     198    0
user_drv              <0.63.0>     user_drv:server/2               685540    0
```
This was done with the following configuration on the bitcoinServer file:
```
-define(NUM_THREADS_SERVERS, 2).
-define(NUM_SERVER_WORKERS, 50).
```
