<?php
/*
 * IPWorks MQ 2024 PHP Edition - Sample Project
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
require_once('../include/ipworksmq_amazonsqs.php');
require_once('../include/ipworksmq_const.php');
?>
<?php

class myAmazonSQS extends IPWorksMQ_AmazonSQS 
{
  function fireQueue($param) 
  {
    echo "Queue ID: " . $param['queueid'] . "\n";
    echo "URL " . $param['url'] . "\n";
  }
    
  function fireMessage($param) 
  {
    echo "message ID: " . $param['messageid'] . "\n";
    echo "message Data: " . $param['messagedata'] . "\n";
    echo "receipt handle: " . $param['receipthandle'] . "\n";
  }
} 

function printMenu() 
{
  echo "\n?\t-\tHelp\n" .
    "cd\t-\tSelect Queue\n" .
    "del\t-\tDelete Queue\n" .
    "mk\t-\tCreate Queue\n" .
    "ls\t-\tList Queues\n" .
    "lsmsg\t-\tList Messages\n" .
    "delmsg\t-\tDelete Message\n" .
    "mkmsg\t-\tCreate Message\n" .
    "q\t-\tQuit\n";
}

$queue = new myAmazonSQS();
$command = "";
$myQueueId = "";

if ($argc < 4) 
{
  echo "usage: amazon_sqs_queue_manager.php -a Access_Key -s Secret_Key \n" .
    "Access_Key: the access key found on your AWS console\n" .
    "Secret_Key: the secret key generated after the access key\n";

  exit(0);
}

$arg = parseArgs($argc, $argv);

$queue->setAccessKey($arg["a"]);
$queue->setSecretKey($arg["s"]);

printMenu();
echo "\nAvailable queues:\n";
$queue->doListQueues();

while (true) 
{
  echo "\nEnter command: ";
  $command = trim(fgets(STDIN));
    
  try {
    if ($command == "cd") 
    {
      echo "Enter queue ID: ";
      $myQueueId = trim(fgets(STDIN));
    }
    elseif ($command == "del") 
    {
      echo "Delete queue with ID: ";
      $queue->doDeleteQueue(trim(fgets(STDIN)));
    } 
    elseif ($command == "mk") 
    {
      echo "Enter queue name: ";
      echo "Queue created with Id: " . $queue->doCreateQueue(trim(fgets(STDIN))) . "\n";
    } 
    elseif ($command == "ls") 
    {
      $queue->doListQueues();
    } 
    elseif ($command == "lsmsg")
    {
      $queue->doListMessages($myQueueId);
    } 
    elseif ($command == "delmsg") 
    {
      echo "Enter receipt Handle: ";
      $queue->doDeleteMessage($myQueueId, trim(fgets(STDIN)));
    } 
    elseif ($command == "mkmsg") 
    {
      echo "Enter message data:";
      echo "Message created with Id: " . $queue->doCreateMessage($myQueueId, trim(fgets(STDIN))) . "\n";
    } 
    elseif ($command == "q")
    {
      exit(0);
    } 
    elseif ($command == "?") 
    {
      printMenu();
    } 
    else 
    {
      echo "not an option!\n";
      printMenu();
    }
  } 
  catch (Exception $e) 
  {
    echo $e->getMessage() . "\n";
    exit(1);
  }
}

function parseArgs($argc, $argv)
{
  $map = array();

  for ($i = 0; $i < $argc; $i++)
  {
    if ($argv[$i][0] == "-")
    {
      if ($i + 1 < $argc && ($argv[$i + 1][0] != "-"))
      {
        $argv[$i] = substr($argv[$i], 1, strlen($argv[$i]));
        $map[$argv[$i]] = $argv[$i + 1];
        $i++;
      }
      else 
      {
        $argv[$i] = substr($argv[$i], 1, strlen($argv[$i]));
        $map[$argv[$i]] = "";
      }
    }
    else 
    {
      $map[$i] = "";
    }
  }
  return $map;
}