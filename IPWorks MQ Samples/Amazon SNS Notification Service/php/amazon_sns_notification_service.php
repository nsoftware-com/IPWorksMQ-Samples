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
require_once('../include/ipworksmq_amazonsns.php');
require_once('../include/ipworksmq_const.php');
?>
<?php
class myAmazonSNS extends IPWorksMQ_AmazonSNS
{
  public function fireTopicList($param)
  {
    echo $param['topicarn'] . "\n";
  }

  public function fireSubscriptionList($param)
  {
    echo "\ntopic ARN: " . $param['topicarn'] . "\n";
    echo "subscription ARN: " . $param['subscriptionarn'] . "\n";
    echo "endpoint: " . $param['endpoint'] . "\n";
    echo "owner: " . $param['owner'] . "\n";

    switch ($param['protocol']) {
      case 0:
        echo "EndPoint protocol: email\n";
        break;
      case 1:
        echo "EndPoint protocol: email-json\n";
        break;
      case 2:
        echo "EndPoint protocol: http\n";
        break;
      case 3:
        echo "EndPoint protocol: https\n";
        break;
      case 4:
        echo "EndPoint protocol: SMS\n";
        break;
      case 5:
        echo "EndPoint protocol: SQS\n";
        break;
      default:
        echo "not known protocol\n";
    }

    echo "-----------------------------------------------------------------------------------------\n";
  }
}

function printMenu() 
{
  echo "\r\n?\t-\tHelp\n" .
    "lst\t-\tList Topics\n" .
    "lss\t-\tList Subscriptions\n" .
    "lsst\t-\tList Subscriptions by topic\n" .
    "mk\t-\tCreate Topic\n" .
    "del\t-\tDelete Topic\n" .
    "sub\t-\tSubscribe\n" .
    "unsub\t-\tUnSubscribe\n" .
    "pub\t-\tPublish\n" .
    "q\t-\tQuit\n";
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

$sns = new myAmazonSNS();
$command = "";

if ($argc < 4) 
{
  echo "usage: amazon_sns_notification_service.php -a Access_Key -s Secret_Key \n" .
    "Access_Key: the access key found on your AWS console\n" .
    "Secret_Key: the secret key generated after the access key\n";

  exit(0);
}

$args = parseArgs($argc, $argv);

$sns->setAccessKey($args["a"]);
$sns->setSecretKey($args["s"]);

printMenu();

while (true)
{
  echo "\nEnter command: ";
  $command = trim(fgets(STDIN));

  try {
    switch ($command) {
      case "lst":
        do $sns->doListTopics();
        while ($sns->getTopicMarker() != "");
        break;

      case "lss":
        do $sns->doListSubscriptions();
        while ($sns->getSubscriptionMarker() != "");
        break;
      
      case "lsst":
        echo "Enter topic ARN: ";
        $sns->doListTopicSubscriptions(trim(fgets(STDIN)));
        break;

      case "mk":
        echo "Enter topic name: ";
        echo "Topic ARN for new topic: " . $sns->doCreateTopic(trim(fgets(STDIN))) . "\n";
        break;
      
      case "del":
        echo "Enter topic ARN: ";
        $sns->doDeleteTopic(trim(fgets(STDIN)));
        break;

      case "sub":
        $topicArn = "";
        $endpoint = "";
        $endpointProtocol = 0;

        echo "Enter a topic: \n";
        $topicArn = trim(fgets(STDIN));

        echo "Enter an endpoint (e.g., email address, phone number): \n";
        $endpoint = trim(fgets(STDIN));

        echo "Choose an endpoint protocol: \n";
        echo "0 for email\n";
        echo "1 for email-json\n";
        echo "2 for http\n";
        echo "3 for https\n";
        echo "4 for sms\n";
        echo "5 for sqs\n";

        $endpointProtocol = intval(trim(fgets(STDIN)));

        echo "Subscription ARN for new subscriber: " . $sns->doSubscribe($topicArn, $endpoint, $endpointProtocol);
        break;

      case "unsub":
        echo "Subscription ARN: ";
        $sns->doUnsubscribe(trim(fgets(STDIN)));
        break;

      case "pub":
        $topicArn1 = "";
        $subject = "";
        $message = "";

        echo "Enter a topic: ";
        $topicArn1 = trim(fgets(STDIN));

        echo "Enter subject: ";
        $subject = trim(fgets(STDIN));

        echo "Enter body: ";
        $message = trim(fgets(STDIN));

        echo "Message Id: " . $sns->doPublish($topicArn1, $subject, $message);
        break;
      
      case "q":
        exit(0);

      case "?":
        printMenu();
        break;

      default:
        echo "not an option!\n";
        printMenu();
        break;
    }
  } 
  catch (Exception $e) 
  {
    echo $e->getMessage() . "\n";
    exit(1);
  }
}