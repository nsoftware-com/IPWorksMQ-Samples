/*
 * IPWorks MQ 2024 C++ Edition - Sample Project
 *
 * This sample project demonstrates the usage of IPWorks MQ in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/ipworksmq
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 */

#include <stdio.h>
#include <string>
#include <unordered_map>
#include <stdlib.h>
#include "../../include/amazonsqs.h"
#define BUFF_SIZE 500

class MySQS : public AmazonSQS
{
public:
  virtual int FireQueue(AmazonSQSQueueEventParams* e)
  {
    printf("Queue ID: %s\nURL: %s\n\n", e->QueueId, e->URL);
    return 0;
  }

  virtual int FireMessage(AmazonSQSMessageEventParams* e)
  {
    printf("ID: %s\nData: %s\nReceipt handle: %s\n\n", e->MessageId, e->MessageData, e->ReceiptHandle);
    return 0;
  }

  virtual int FireError(AmazonSQSErrorEventParams* e)
  {
    printf("Error: %s\n\n", e->Description);
    return 0;
  }
};

void printMenu();
void toLowerCase(char* str);
std::unordered_map<std::string, std::string> parseArgs(int argc, char* argv[]);

int main(int argc, char* argv[])
{
  MySQS queue;
  char buffer[BUFF_SIZE];
  char myQueueId[BUFF_SIZE] = "";

  if (argc < 4)
  {
    printf("usage: sqs.cpp -a Access_Key -s Secret_Key \n" \
      "Access_Key: the access key found on your AWS console\n" \
      "Secret_Key: the secret key generated after the access key\n");

    return 0;
  }

  std::unordered_map<std::string, std::string> arg = parseArgs(argc, argv);

  queue.SetAccessKey(arg.at("a").c_str());
  queue.SetSecretKey(arg.at("s").c_str());

  printMenu();
  printf("\nAvailable queues:\n");
  queue.ListQueues();

  while (true)
  {
    int retCode = 0;
    printf("\nEnter command: ");
    scanf("%[^\n]", buffer);

    if (!strcmp(buffer, "cd"))
    {
      printf("Enter queue ID: ");
      getchar();
      scanf("%[^\n]", myQueueId);
    }
    else if (!strcmp(buffer, "del"))
    {
      printf("Delete queue with ID: ");
      getchar();
      scanf("%[^\n]", buffer);
      retCode = queue.DeleteQueue(buffer);
      if (retCode != 0)
      {
        printf("Error: [%i] %s\n\n", retCode, queue.GetLastError());
      }
    }
    else if (!strcmp(buffer, "mk"))
    {
      printf("Enter queue name: ");
      getchar();
      scanf("%[^\n]", buffer);

      char* queueID = queue.CreateQueue(buffer);
      if (queueID == nullptr)
      {
        printf("Something went wrong!");
        getchar();
        continue;
      }

      printf("Queue created with ID: %s", queueID);
    }
    else if (!strcmp(buffer, "ls"))
    {
      retCode = queue.ListQueues();
      if (retCode != 0)
      {
        printf("Error: [%i] %s\n\n", retCode, queue.GetLastError());
      }
    }
    else if (!strcmp(buffer, "lsmsg"))
    {
      retCode = queue.ListMessages(myQueueId);
      if (retCode != 0)
      {
        printf("Error: [%i] %s\n\n", retCode, queue.GetLastError());
      }
    }
    else if (!strcmp(buffer, "delmsg"))
    {
      printf("Enter receipt handle: ");
      getchar();
      scanf("%[^\n]", buffer);
      retCode = queue.DeleteMessage(myQueueId, buffer);
      if (retCode != 0)
      {
        printf("Error: [%i] %s\n\n", retCode, queue.GetLastError());
      }
    }
    else if (!strcmp(buffer, "mkmsg"))
    {
      printf("Enter message data: ");
      getchar();
      scanf("%[^\n]", buffer);

      char* msgID = queue.CreateMessage(myQueueId, buffer);
      if (msgID == nullptr)
      {
        printf("Something went wrong!");
        getchar();
        continue;
      }
    }
    else if (!strcmp(buffer, "q"))
    {
      return 0;
    }
    else if (!strcmp(buffer, "?"))
    {
      printMenu();
    }
    else
    {
      printf("Command not recognized!");
      printMenu();
    }
    getchar();
  }
  return 0;
}

void printMenu()
{
  printf("\r\n?\t-\tHelp\n" \
    "cd\t-\tSelect Queue\n" \
    "del\t-\tDelete Queue\n" \
    "mk\t-\tCreate Queue\n" \
    "ls\t-\tList Queues\n" \
    "lsmsg\t-\tList Messages\n" \
    "delmsg\t-\tDelete Message\n" \
    "mkmsg\t-\tCreate Message\n" \
    "q\t-\tQuit\n");
}

std::unordered_map<std::string, std::string> parseArgs(int argc, char* argv[])
{
  std::unordered_map<std::string, std::string> map;
  for (int i = 0; i < argc; i++)
  {
    if (argv[i][0] == '-')
    {
      if (i + 1 < argc && (argv[i + 1][0] != '-'))
      {
        char* newStr = argv[i] + 1;
        toLowerCase(newStr);
        map[newStr] = argv[i + 1];
        i++;
      }
      else
      {
        char* newStr = argv[i] + 1;
        toLowerCase(newStr);
        map[newStr] = "";
      }
    }
    else
    {
      map[std::to_string(i)] = argv[i];
    }
  }
  return map;
}

void toLowerCase(char* str)
{
  for (size_t i = 0; i < std::strlen(str); ++i) {
    str[i] = std::tolower(static_cast<unsigned char>(str[i]));
  }
}

