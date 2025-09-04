/*
 * IPWorks MQ 2024 JavaScript Edition - Sample Project
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
 
const readline = require("readline");
const ipworksmq = require("@nsoftware/ipworksmq");

if(!ipworksmq) {
  console.error("Cannot find ipworksmq.");
  process.exit(1);
}
let rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout
});

const { exit } = require("process");

const promt = (query) => new Promise((resolve) => rl.question(query, resolve));
var sns = new ipworksmq.amazonsns();

sns.on("TopicList", function (e) {
  console.log(e.topicArn);
});

sns.on("SubscriptionList", function (e) {
  console.log("\ntopic ARN: " + e.topicArn);
  console.log("subscription ARN: " + e.subscriptionArn);
  console.log("endpoint: " + e.endpoint);
  console.log("owner: " + e.owner);

  switch (e.protocol) {
    case 0:
      console.log("EndPoint protocol: email");
      break;
    case 1:
      console.log("EndPoint protocol: email-json");
      break;
    case 2:
      console.log("EndPoint protocol: http");
      break;
    case 3:
      console.log("EndPoint protocol: https");
      break;
    case 4:
      console.log("EndPoint protocol: SMS");
      break;
    case 5:
      console.log("EndPoint protocol: SQS");
      break;
    default:
      console.log("not known protocol");
  }
  console.log("-----------------------------------------------------------------------------------------");
});

main();

function printMenu()
{
  console.log(`    ? \t-\tHelp
    lst\t-\tList Topics
    lss\t-\tList Subscriptions
    lsst\t-\tList Subscriptions by Topic
    mk\t-\tCreate Topic
    del\t-\tDelete Topic
    sub\t-\tSubscribe
    unsub -\tUnSubscribe
    pub\t-\tPublish
    q\t-\tQuit`);    
}

function parseArgs(argv)
{
  var map = {};

  for (let i = 0; i < argv.length; i++) {
    if (argv[i][0] == "-")
    {
      if (i + 1 < argv.length && (argv[i + 1][0] != "-"))
      {
        argv[i] = (argv[i].slice(1)).toLowerCase();
        map[argv[i]] = argv[i + 1];
        i++;
      }
      else
      {
        argv[i] = (argv[i].slice(1)).toLowerCase();
        map[argv[i]] = "";
      }
    }
    else
    {
      map[i] = argv[i];
    }
  }

  return map;
}

async function main() 
{
  if (process.argv.length < 6) {
    console.log("usage: sns.js -a Access_Key -s Secret_Key \n" +
      "Access_Key: the access key found on your AWS console\n" +
      "Secret_Key: the secret key generated after the access key\n");  

    exit();
  }

  const args = parseArgs(process.argv);
  sns.setAccessKey(args["a"]);
  sns.setSecretKey(args["s"]);
  
  printMenu();
  console.log("\nEnter command: ");

  rl.on("line", async function (line)
  {
    switch (line.toLowerCase()) {
      case "lst":
        do await sns.listTopics();
        while (sns.getTopicMarker() != "");
        break;
      
      case "lss":
        do await sns.listSubscriptions();
        while (sns.getSubscriptionMarker() != "");
        break;

      case "lsst":
        await sns.listTopicSubscriptions(await promt("Enter topic ARN: "));
        break;

      case "mk":
        console.log("Topic ARN for new topic: " + await sns.createTopic(await promt("Enter topic name: ")).catch(function (e)
        {
          console.log(e.message);
        }));
        break;

      case "del":
        await sns.deleteTopic(await promt("Enter topic name: ")).catch(function (e)
        {
          console.log(e.message);
        });
        break;

      case "sub":
        var topicArn;
        var endpoint;
        var endpointProtocol;

        topicArn = await promt("Enter a topic: ");
        endpoint = await promt("Enter an endpoint i.e email add, phone nr..: ");
        endpointProtocol = await promt("Choose a endpoint protocol: \n" +
          "0 for email\n" +
          "1 for email-json\n" +
          "2 for http\n" +
          "3 for https\n" +
          "4 for sms\n" +
          "5 for sqs\n");
        
        console.log(topicArn);
        console.log(endpoint);
        console.log(endpointProtocol);

        console.log("Subscription ARN for new subscriber: " + await sns.subscribe(topicArn, endpoint, parseInt(endpointProtocol)).catch(function (e)
        {
          console.log(e.message);
        }));
        break;

      case "unsub":
        await sns.unsubscribe(await promt("Subscription ARN: ")).catch(function (e)
        {
          console.log(e.message);
        });
        break;

      case "pub":
        var topicArn1;
        var subject;
        var message;
        
        topicArn1 = await promt("Enter a topic: ");
        subject = await promt("Enter subject: ");
        message = await promt("Enter body: ");

        console.log("Message Id: " + await sns.publish(topicArn1, subject, message).catch(function (e)
        {
          console.log(e.message);
        }));
        break;

      case "q":
        exit();

      default:
        console.log("not an option!");
        printMenu();
        break;
    }
    console.log("\nEnter command: ");
  });
}

function prompt(promptName, label, punctuation, defaultVal)
{
  lastPrompt = promptName;
  lastDefault = defaultVal;
  process.stdout.write(`${label} [${defaultVal}]${punctuation} `);
}
