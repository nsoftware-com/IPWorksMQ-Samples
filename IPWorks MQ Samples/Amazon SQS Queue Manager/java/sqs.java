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
import ipworksmq.*;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Map;

public class sqs extends ConsoleDemo
{
  public static void main(String[] args)
  {
    try
    {
      new sqs(args);
    }
    catch (Exception e)
    {
      System.out.println(e);
    }
  }

  public sqs(String[] args)
  {
    AmazonSQS queue = new AmazonSQS();
    String buffer = "";
    String myQueueId = "";

    if (args.length < 4)
    {
      System.out.println("usage: sqs.java -a Access_Key -s Secret_Key \n" +
              "Access_Key: the access key found on your AWS console\n" +
              "Secret_Key: the secret key generated after the access key\n");

      return;
    }

    Map<String, String> arg = parseArgs(args);

    try
    {
      queue.addAmazonSQSEventListener(new SqsEvent());
      queue.setAccessKey(arg.get("a"));
      queue.setSecretKey(arg.get("s"));

      printMenu();
      System.out.println("\nAvailable queues:");
      queue.listQueues();

      while (true) 
      {
        System.out.print("\nEnter command: ");
        buffer = input().toLowerCase();

        try 
        {
          switch (buffer) {
            case "cd":
              System.out.print("Enter queue ID: ");
              myQueueId = input();
              break;
            case "del":
              System.out.print("Delete queue with ID: ");
              queue.deleteQueue(input());
              break;
            case "mk":
              System.out.print("Enter queue name: ");
              System.out.println("Queue created with ID: " + queue.createQueue(input()));
              break;
            case "ls":
              queue.listQueues();
              break;
            case "lsmsg":
              queue.listMessages(myQueueId);
              break;
            case "delmsg":
              System.out.print("Enter receipt handle: ");
              queue.deleteMessage(myQueueId, input());
              break;
            case "mkmsg":
              System.out.print("Enter message data: ");
              System.out.println("Message created with ID: " + queue.createMessage(myQueueId, input()));
              break;
            case "?":
              printMenu();
              break;
            case "q":
              System.exit(0);
              break;
            default:
              System.out.println("Command not recognized!");
              printMenu();
          }
        }
        catch (IPWorksMQException e)
        {
          System.out.println(e);
        }
      }
    }
    catch (Exception e)
    {
      System.err.println(e);
    }
  }

  private void printMenu()
  {
    System.out.println("\r\n?\t-\tHelp\n" +
      "cd\t-\tSelect Queue\n" +
      "del\t-\tDelete Queue\n" +
      "mk\t-\tCreate Queue\n" +
      "ls\t-\tList Queues\n" +
      "lsmsg\t-\tList Messages\n" +
      "delmsg\t-\tDelete Message\n" +
      "mkmsg\t-\tCreate Message\n" +
      "q\t-\tQuit\n");
  }
}

class SqsEvent implements AmazonSQSEventListener
{
  @Override
  public void connected(AmazonSQSConnectedEvent amazonSQSConnectedEvent) { }

  @Override
  public void disconnected(AmazonSQSDisconnectedEvent amazonSQSDisconnectedEvent) { }

  @Override
  public void endTransfer(AmazonSQSEndTransferEvent amazonSQSEndTransferEvent) { }

  @Override
  public void error(AmazonSQSErrorEvent amazonSQSErrorEvent) { }

  @Override
  public void header(AmazonSQSHeaderEvent amazonSQSHeaderEvent) { }

  @Override
  public void message(AmazonSQSMessageEvent amazonSQSMessageEvent)
  {
    System.out.println("message ID: " + amazonSQSMessageEvent.messageId);
    System.out.println("message Data: " + amazonSQSMessageEvent.messageData);
    System.out.println("receipt handle: " + amazonSQSMessageEvent.receiptHandle);
  }

  @Override
  public void queue(AmazonSQSQueueEvent amazonSQSQueueEvent)
  {
    System.out.println("Queue ID: " + amazonSQSQueueEvent.queueId);
    System.out.println("URL: " + amazonSQSQueueEvent.URL);
  }

  @Override
  public void SSLServerAuthentication(AmazonSQSSSLServerAuthenticationEvent amazonSQSSSLServerAuthenticationEvent) { }

  @Override
  public void SSLStatus(AmazonSQSSSLStatusEvent amazonSQSSSLStatusEvent) { }

  @Override
  public void startTransfer(AmazonSQSStartTransferEvent amazonSQSStartTransferEvent) { }

  @Override
  public void status(AmazonSQSStatusEvent amazonSQSStatusEvent) { }

  @Override
  public void transfer(AmazonSQSTransferEvent amazonSQSTransferEvent) { }
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




