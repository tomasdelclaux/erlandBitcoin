# erlandBitcoin
Hashing in erland for bitcoins

## INTRODUCTION
Distributed system for mining bitcoins in erlang.

## EXAMPLE 1
Quick explanation of example 1

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
erl -name node@hostname2.com```
