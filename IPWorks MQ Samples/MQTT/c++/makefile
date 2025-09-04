#
# IPWorks MQ 2024 C++ Edition - Sample Project
#
# This sample project demonstrates the usage of IPWorks MQ in a 
# simple, straightforward way. It is not intended to be a complete 
# application. Error handling and other checks are simplified for clarity.
#
# www.nsoftware.com/ipworksmq
#
# This code is subject to the terms and conditions specified in the 
# corresponding product license agreement which outlines the authorized 
# usage and restrictions.
#

ifeq ($(shell uname -s), Darwin) # MACOS
R_PATH=-Wl,-rpath,../../lib64/,-rpath,.

FRAMEWORK = -framework Carbon -framework Security
LD_FLAGS = -lipworksmq.24.0 -L../../lib64/ -lresolv
LD_FLAGS_SRC = -lz -lresolv
OS_CFLAGS = -D UNIX -arch arm64
MACOS = "darwin% Darwin% macos%"

all:
	g++ $(OS_CFLAGS) -D UNIX -o mqtt mqtt.cpp  -I../../include/ $(LD_FLAGS) $(FRAMEWORK) $(R_PATH)

src: ../../src/ipworksmq.o
	g++ $(OS_CFLAGS) -o mqtt ../../src/ipworksmq.o mqtt.cpp  $(LD_FLAGS_SRC) $(FRAMEWORK)

../../src/ipworksmq.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -arch arm64 -o ../../src/ipworksmq.o ../../src/ipworksmq.cpp

else # LINUX
CPU=$(shell uname -m)
# It's assumed that a product is always built for Linux/x64 platform
ifeq ($(CPU), $(filter $(CPU), i386 i686))
  LIB=lib
endif
ifeq ($(LIB),)
  LIB=lib64
endif

R_PATH=-Wl,-rpath,../../$(LIB)/,-rpath,.
LD_FLAGS = -lipworksmq  -L../../$(LIB)/ 
LD_FLAGS_SRC = -lz 

all:
	g++ -D UNIX  -o mqtt mqtt.cpp  -I../../include/ $(LD_FLAGS) $(R_PATH)

src: ../../src/ipworksmq.o
	g++ -D UNIX -o mqtt ../../src/ipworksmq.o mqtt.cpp  -I../../include/ $(LD_FLAGS_SRC)

../../src/ipworksmq.o:
	g++ -c -fno-exceptions -fno-rtti -fPIC -o ../../src/ipworksmq.o ../../src/ipworksmq.cpp

clean:
	rm -f ../../src/*.o
	rm -f mqtt *.o
endif