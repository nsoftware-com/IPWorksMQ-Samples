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
#include <stdlib.h>
#include <unordered_map>
#include "../../include/amazonsns.h"
#define MAX_LEN 500

class MySNS : public AmazonSNS
{
public:
  virtual int FireTopicList(AmazonSNSTopicListEventParams* e)
  {
    printf("%s\n", e->TopicArn);
    return 0;
  }

  virtual int FireSubscriptionList(AmazonSNSSubscriptionListEventParams* e)
  {
    printf("\ntopic ARN: %s\n", e->TopicArn);
    printf("subscription ARN: %s\n", e->SubscriptionArn);
    printf("endpoint: %s\n", e->Endpoint);
    printf("owner: %s\n", e->Owner);

    switch (e->Protocol) {
    case 0:
      printf("EndPoint protocol: email\n");
      break;
    case 1:
      printf("EndPoint protocol: email-json\n");
      break;
    case 2:
      printf("EndPoint protocol: http\n");
      break;
    case 3:
      printf("EndPoint protocol: https\n");
      break;
    case 4:
      printf("EndPoint protocol: SMS\n");
      break;
    case 5:
      printf("EndPoint protocol: SQS\n");
      break;
    default:
      printf("not known protocol\n");
      break;
    }

    printf("-----------------------------------------------------------------------------------------\n");

    return 0;
  }

  virtual int FireError(AmazonSNSErrorEventParams* e)
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
  MySNS sns;
  char buffer[MAX_LEN];

  if (argc < 4)
  {
    printf("usage: sns.cpp -a Access_Key -s Secret_Key \n" \
      "Access_Key: the access key found on your AWS console\n" \
      "Secret_Key: the secret key generated after the access key\n");

    return 0;
  }

  std::unordered_map<std::string, std::string> map = parseArgs(argc, argv);

  sns.SetAccessKey(map.at("a").c_str());
  sns.SetSecretKey(map.at("s").c_str());

  printMenu();

  while (true)
  {
    int retCode = 0;
    printf("\nEnter command: ");
    scanf("%[^\n]", buffer);

    if (!strcmp(buffer, "lst"))
    {
      do
      {
        retCode = sns.ListTopics();
        if (retCode != 0)
        {
          printf("Error: [%i] %s\n\n", retCode, sns.GetLastError());
          break;
        }
      } while (strlen(sns.GetTopicMarker()) != 0);
    }
    else if (!strcmp(buffer, "lss"))
    {
      do
      {
        retCode = sns.ListSubscriptions();
        if (retCode != 0)
        {
          printf("Error: [%i] %s\n\n", retCode, sns.GetLastError());
          break;
        }
      } while (strlen(sns.GetSubscriptionMarker()) != 0);
    }
    else if (!strcmp(buffer, "lsst"))
    {
      printf("Enter topic ARN: ");
      getchar();
      scanf("%[^\n]", buffer);

      retCode = sns.ListTopicSubscriptions(buffer);
      if (retCode != 0)
      {
        printf("Error: [%i] %s\n\n", retCode, sns.GetLastError());
      }
    }
    else if (!strcmp(buffer, "mk"))
    {
      char* topicArn;
      printf("Enter topic name: ");
      getchar();
      scanf("%[^\n]", buffer);

      topicArn = sns.CreateTopic(buffer);
      if (strlen(topicArn) == 0)
      {
        getchar();
        printf("Something went wrong!");
        continue;
      }
      printf("Topic ARN for new topic: %s\n", topicArn);
    }
    else if (!strcmp(buffer, "del"))
    {
      printf("Enter topic ARN: ");
      getchar();
      scanf("%[^\n]", buffer);
      retCode = sns.DeleteTopic(buffer);
      if (retCode != 0)
      {
        printf("Error: [%i] %s\n\n", retCode, sns.GetLastError());
      }
    }
    else if (!strcmp(buffer, "sub"))
    {
      char topicArn[MAX_LEN];
      char endpoint[MAX_LEN];
      char* subArn;
      int endpointProtocol;

      printf("Enter a topic: ");
      getchar();
      scanf("%[^\n]", topicArn);

      printf("Enter an endpoint i.e email add, phone nr..: ");
      getchar();
      scanf("%[^\n]", endpoint);

      printf("Choose a endpoint protocol: \n" \
        "0 for email\n" \
        "1 for email-json\n" \
        "2 for http\n" \
        "3 for https\n" \
        "4 for sms\n" \
        "5 for sqs\n");

      scanf("%d", &endpointProtocol);

      subArn = sns.Subscribe(topicArn, endpoint, endpointProtocol);
      if (strlen(subArn) == 0)
      {
        printf("Something went wrong!");
        getchar();
        continue;
      }
      printf("Subscription ARN for new subscriber: %s\n", subArn);
    }
    else if (!strcmp(buffer, "unsub"))
    {
      printf("Subscription ARN: ");
      getchar();
      scanf("%[^\n]", buffer);
      retCode = sns.Unsubscribe(buffer);
      if (retCode != 0)
      {
        printf("Error: [%i] %s\n\n", retCode, sns.GetLastError());
      }
    }
    else if (!strcmp(buffer, "pub"))
    {
      char topicArn[MAX_LEN];
      char subject[MAX_LEN];
      char message[MAX_LEN];
      char* msgId;

      printf("Enter a topic: ");
      getchar();
      scanf("%[^\n]", topicArn);

      printf("Enter subject: ");
      getchar();
      scanf("%[^\n]", subject);

      printf("Enter body: ");
      getchar();
      scanf("%[^\n]", message);

      msgId = sns.Publish(topicArn, subject, message);
      if (strlen(msgId) == 0)
      {
        printf("Something went wrong!");
        getchar();
        continue;
      }
      printf("Message Id: %s", msgId);
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
      printf("not an option!");
      printMenu();
    }
    getchar();
  }
  return 0;
}

void printMenu()
{
  printf("\r\n?\t-\tHelp\n" \
    "lst\t-\tList Topics\n" \
    "lss\t-\tList Subscriptions\n" \
    "lsst\t-\tList Subscriptions by topic\n" \
    "mk\t-\tCreate Topic\n" \
    "del\t-\tDelete Topic\n" \
    "sub\t-\tSubscribe\n" \
    "unsub\t-\tUnSubscribe\n" \
    "pub\t-\tPublish\n" \
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

