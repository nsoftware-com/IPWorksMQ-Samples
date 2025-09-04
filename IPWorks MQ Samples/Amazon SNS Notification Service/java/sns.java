/*
 * IPWorks MQ 2024 Java Edition - Sample Project
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

import java.io.*;
import java.io.*;
import java.io.*;
import ipworksmq.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;
 
public class sns extends ConsoleDemo
{
  public static void main(String[] args)
  {
    AmazonSNS sns = new AmazonSNS();
    String buffer = "";

    if (args.length < 4)
    {
      System.out.println("usage: sns.java -a Access_Key -s Secret_Key \n" +
        "Access_Key: the access key found on your AWS console\n" +
        "Secret_Key: the secret key generated after the access key\n");

      return;
    }

    Map<String, String> param = parseArgs(args);
 
    try
    {
      sns.addAmazonSNSEventListener(new SnsEvents());
      sns.setAccessKey(param.get("a"));
      sns.setSecretKey(param.get("s"));
 
      printMenu();
 
      while (true)
      {
        buffer = prompt("\nEnter command").toLowerCase();
 
        try
        {
          switch (buffer)
          {
            case "?":
              printMenu();
              break;
            case "lst":
              do sns.listTopics();
              while (!sns.getTopicMarker().isEmpty());
              break;
            case "lss":
              do sns.listSubscriptions();
              while (!sns.getSubscriptionMarker().isEmpty());
              break;
            case "lsst":
              sns.listTopicSubscriptions(prompt("Enter topic ARN"));
              break;
            case "mk":
              System.out.println("Topic ARN for new topic: " + sns.createTopic(prompt("Enter topic name")));
              break;
            case "del":
              sns.deleteTopic(prompt("Enter topic ARN"));
              System.out.println("success!");
              break;
            case "sub":
              String topicArn;
              String endpoint;
              int endpointProtocol;
 
              topicArn = prompt("Enter a topic");
              endpoint = prompt("Enter an endpoint i.e email add, phone nr..");
 
              endpointProtocol = Integer.parseInt(prompt("Choose a endpoint protocol: \n" +
                                    "0 for email\n" +
                                    "1 for email-json\n" +
                                    "2 for http\n" +
                                    "3 for https\n" +
                                    "4 for sms\n" +
                                    "5 for sqs"));
 
              System.out.println("Subscription ARN for new subscriber: " + sns.subscribe(topicArn, endpoint, endpointProtocol));
              break;
            case "unsub":
              sns.unsubscribe(prompt("Subscription ARN"));
              System.out.println("success!");
              break;
            case "pub":
              String topicArn1;
              String subject;
              String message;
 
              topicArn1 = prompt("Enter a topic");
              subject = prompt("Enter subject");
              message = prompt("Enter body");
 
              System.out.println("Message Id: " + sns.publish(topicArn1, subject, message));
              break;
            case "q":
              System.exit(0);
            default:
              System.out.println("not an option!");
              printMenu();
          }
        }
        catch (IPWorksMQException e)
        {
          System.err.println(e);
        }
      }
    }
    catch (Exception e)
    {
      System.err.println(e);
    }
  }
 
  private static void printMenu()
  {
    System.out.println("\r\n?\t-\tHelp\n" +
      "lst\t-\tList Topics\n" +
      "lss\t-\tList Subscriptions\n" +
      "lsst\t-\tList Subscriptions by Topic\n" +
      "mk\t-\tCreate Topic\n" +
      "del\t-\tDelete Topic\n" +
      "sub\t-\tSubscribe\n" +
      "unsub\t-\tUnSubscribe\n" +
      "pub\t-\tPublish\n" +
      "q\t-\tQuit\n");
  }
}
 
class SnsEvents implements AmazonSNSEventListener
{
  @Override
  public void error(AmazonSNSErrorEvent amazonSNSErrorEvent) { }
  
  @Override
  public void log(AmazonSNSLogEvent amazonSNSLogEvent) { }
  
  @Override
  public void SSLServerAuthentication(AmazonSNSSSLServerAuthenticationEvent amazonSNSSSLServerAuthenticationEvent) { }
  
  @Override
  public void SSLStatus(AmazonSNSSSLStatusEvent amazonSNSSSLStatusEvent) { }
 
  @Override
  public void subscriptionList(AmazonSNSSubscriptionListEvent amazonSNSSubscriptionListEvent)
  {
    System.out.println("\ntopic ARN: " + amazonSNSSubscriptionListEvent.topicArn);
    System.out.println("subscription ARN: " + amazonSNSSubscriptionListEvent.subscriptionArn);
    System.out.println("endpoint: " + amazonSNSSubscriptionListEvent.endpoint);
    System.out.println("owner: " + amazonSNSSubscriptionListEvent.owner);
    switch (amazonSNSSubscriptionListEvent.protocol)
    {
      case 0:
        System.out.println("EndPoint protocol: email");
        break;
      case 1:
        System.out.println("EndPoint protocol: email-json");
        break;
      case 2:
        System.out.println("EndPoint protocol: http");
        break;
      case 3:
        System.out.println("EndPoint protocol: https");
        break;
      case 4:
        System.out.println("EndPoint protocol: SMS");
        break;
      case 5:
        System.out.println("EndPoint protocol: SQS");
        break;
      default:
        System.out.println("not known protocol");
    }
    System.out.println("-----------------------------------------------------------------------------------------");
  }
 
  @Override
  public void topicList(AmazonSNSTopicListEvent amazonSNSTopicListEvent)
  {
    System.out.println(amazonSNSTopicListEvent.topicArn);
  }
} 
class ConsoleDemo {
  private static BufferedReader bf = new BufferedReader(new InputStreamReader(System.in));

  static String input() {
    try {
      return bf.readLine();
    } catch (IOException ioe) {
      return "";
    }
  }
  static char read() {
    return input().charAt(0);
  }

  static String prompt(String label) {
    return prompt(label, ":");
  }
  static String prompt(String label, String punctuation) {
    System.out.print(label + punctuation + " ");
    return input();
  }
  static String prompt(String label, String punctuation, String defaultVal) {
      System.out.print(label + " [" + defaultVal + "]" + punctuation + " ");
      String response = input();
      if (response.equals(""))
        return defaultVal;
      else
        return response;
  }

  static char ask(String label) {
    return ask(label, "?");
  }
  static char ask(String label, String punctuation) {
    return ask(label, punctuation, "(y/n)");
  }
  static char ask(String label, String punctuation, String answers) {
    System.out.print(label + punctuation + " " + answers + " ");
    return Character.toLowerCase(read());
  }

  static void displayError(Exception e) {
    System.out.print("Error");
    if (e instanceof IPWorksMQException) {
      System.out.print(" (" + ((IPWorksMQException) e).getCode() + ")");
    }
    System.out.println(": " + e.getMessage());
    e.printStackTrace();
  }

  /**
   * Takes a list of switch arguments or name-value arguments and turns it into a map.
   */
  static java.util.Map<String, String> parseArgs(String[] args) {
    java.util.Map<String, String> map = new java.util.HashMap<String, String>();
    
    for (int i = 0; i < args.length; i++) {
      // Add a key to the map for each argument.
      if (args[i].startsWith("-")) {
        // If the next argument does NOT start with a "-" then it is a value.
        if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
          // Save the value and skip the next entry in the list of arguments.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), args[i + 1]);
          i++;
        } else {
          // If the next argument starts with a "-", then we assume the current one is a switch.
          map.put(args[i].toLowerCase().replaceFirst("^-+", ""), "");
        }
      } else {
        // If the argument does not start with a "-", store the argument based on the index.
        map.put(Integer.toString(i), args[i].toLowerCase());
      }
    }
    return map;
  }
}



