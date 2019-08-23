---
layout: post
title: "Workflow Automation"
date: 2019-08-23 11:27 
comments: true
categories: 
---

In 2019 the information overflow has become a problem, not being able to fine tune my information channel has annoyed me for sometime, and they could be summarized as follows:

1. The service would mixed-in the Ads with the useful information you need. For example, if you install a comics app, apart from the newly released episode it also sends you the recommendation you are not interested into.
2. Even the service provides certain level of customization on what should be pushed. It's not 100% matching what you really need or simply lacking some of the contents you care the most. 
3. It's the same for RSS, it only provides RSS for everything but you might be only interested into one specific category from this site.
4. Not to mention that the site doesn't provide RSS at all and contains all of the Ads. It needs you to keep polling the site so that they could generate revenue from the Ads, or they could track your usage since the content is behind the paywall and requires your login.
5. Push notification could interrupt you at anytime, you could set it to be muted at certain time range but it is the OS level and not able to customise it to what you would prefer.

I would like to have 100% control about my information consumption. 

1. I could set the push notification to be delivered at certain time so that the context switch is reduced. The stress on information overflow could be eased without worrying that I would lose track of some of the information
2. Customise the push and pull model to my like.
3. Reduce the overhead of polling so that I could reclaim my hours to focus on the important stuffs. If I could reclaim 30 minutes every week, I could reclaim 2 hours a month. Given that a ballpark calculation that office workers has about 112 hours of disposable free time every month, reclaiming 2 hours is significant. (every work day for 2 hours on average and 9 hours each day for weekend excluding that you would like to get up late and fool around to get healthy rest, then in total is 28 hours per week and 112 hours per month).

Initially I look up the site to see if there is any pre-existing service that I could leverage by simply just paying to automate the tasks, I tried out a few and decided to roll my own solution. Here are the ones I tried.

1. [IFTTT](https://ifttt.com/) has been around for a long time, but it is only for If-then-Else and the integration it provided are too simple. The only useful thing I could find is the task for putio. 
2. iPhone's Workflow is only for iPhone specific macro, it is not what I would like to have a constantly monitoring service and listening to the trigger.
3. Azure Workflow is better than IFTTT in terms of the complex tasks, and it is pricing model is more friendly if you don't want to pay. However I felt its UI is unintuitive and buggy.
4. The best combination is [Zapier](https://zapier.com/) and [Apify](https://apify.com/). Apify provides crawling site and turn it into the CSV and JSON, you could also set it to be cron jobs as well, if your crawling volume is not high then it's free. Zapier maybe the most affordable integration service provider out there. The rest of them often charges several hundred dollars and it is business oriented. For multi-steps workflow you could chose the $25 a month plan from Zapier to integrate stuffs, and you could basically use Google Sheets as your database to wire everything. It is convenient by clicking through forms and set things up and you could test the setup by replay test. However, 25 dollars a month is simply too expensive for personal usage.

I decided to roll my own solution in the end, the decision was based on

1. It is much much cheaper by paying 5 dollars a month to Virtual Hosting than 25 dollars a month to Zapier.
2. There are still automation that neither Apify nor Zapier could match where I need full programming environment.
3. It's not significantly more complicated to setup headless-chrome and crawl by yourself to a developer. For sure it is not possible for non-tech-savvy group of people.

headless-chrome and puppeteer maybe the best things in these two years to that could make your life easier. It makes the site very hard to tell the difference between bot and your normal activity. Therefore the content that used to be troublesome to crawl is so much easier now. For the content behind paywall you could just load your cookie from the local storage to pretend that you are logged-in and crawl the content. And for the content rendered by javascript you could simply just wait for certain elements to appear then dump the snapshot. As long as you rate-limited your requests, you don't have to worry about the recaptcha since the site looks your activity pretty much like normal users. 

Here is an example of crawling SeekingAlpha.

```
import * as puppeteer from 'puppeteer';
import * as fs from 'fs';
import * as program from 'commander';

program
    .version('0.1.0')
    .option('-c, --cookie [file]', 'Use the cookie to crawl the website')
    .parse(process.argv);

if (program.args.length == 0) {
    program.outputHelp();
    process.exit(0)
}

let url_or_file = program.args[0];

function extractItems(): Array<any> {
    let single_articles = document.querySelectorAll('.author-single-article');

    var comments = [] as any;
    for (let single_article of single_articles) {
        let comment = single_article.querySelector('.author-comment-content');
        let article_link = single_article.querySelector('.article-link');

        if (comment != null && article_link != null) {
            let o: any = {
                "comment": comment.textContent,
                "article_link": "https://seekingalpha.com" + article_link.getAttribute("href"),
                "article_title": article_link.textContent
            };

            comments.push(o);
        }
    }

    return comments
}

async function loadCookie(cookie_path, page): Promise<void> {
    var objects = JSON.parse(fs.readFileSync(program.cookie, 'utf8'));
    if (objects.length) {
        for (let cookie of objects) {
            await page.setCookie(cookie);
        }
    }
}

(async () => {
    const browser = await puppeteer.launch();
    const page = await browser.newPage();

    if (program.cookie && fs.existsSync(program.cookie)) {
        await loadCookie(program.cookie, page);
    }

    if (url_or_file.startsWith('http')) {
        await page.goto(url_or_file, { waitUntil: 'domcontentloaded' });
        await page.waitForSelector('.author-comment-content');
    } else {
        let htmlContent = fs.readFileSync(url_or_file, 'utf-8');
        await page.setContent(htmlContent);
    }

    let items = await page.evaluate(extractItems);
    console.log(JSON.stringify(items));
    await browser.close();
})();
```

It's pretty intuitive and you could deploy the script to your virtual host. And deliver the content to Telegram as push notification. What you need to do is just to create a bot from Telegram and leverage the rubygem to send the text. The richness in Rubygem makes the glue programming quite easy and short. It's not really significantly difficult to an experienced developer to do it than using the service like Zapier.

Since the bandwitdh on Virtual Host is also much faster than your home's ADSL, it is also better to move the large files between services on the server. I could easily use the script to move the file from putio to google drive. 

```
#!/usr/bin/env bash
TARGET_DIR=$HOME/file_pipe

cd $HOME
filename=$(lftp ftp.put.io -u $PUTIO_PASSWD -e "set ftp:ssl-allow no; cd Hook; ls; exit;" 2> /dev/null | awk '{print $9}' | head -1)

if [ -z "$filename" ]
  echo 'no file to pipe'
  exit
fi

lftp ftp.put.io -u $PUTIO_PASSWD -e "set ftp:ssl-allow no; cd Hook; get $filename; exit;" 2> /dev/null
mkdir -p $TARGET_DIR
mv $filename $TARGET_DIR
renamer.rb $TARGET_DIR/$filename

cd $HOME
for f in $(ls $TARGET_DIR)
do
    drive push -no-prompt --files $TARGET_DIR/$f
done

rm -rf $TARGET_DIR
lftp ftp.put.io -u $PUTIO_PASSWD -e "set ftp:ssl-allow no; cd Hook; rm $filename; exit;" 2> /dev/null
```

With these scripts I need to pay zero attention to the contents, the push would be sent to my Telegram, it's all customizable and I could turn off the app notification completely. All I need to make sure there is health check script that would remind me when the server is down (with the warnings also sent by Telegram since I don't need a full solution of health monitoring).

Next milestone I would turn to the browser automation by developing my own browser extension. Due to my heavy usage pattern of Read It Later, I accumulate thousands of links over the years and I am trying to design a workflow that would help me read the information more effective, but not just by dumping the information into Pocket and pretend that I would read it and create a delusion that makes myself feel satisfied. I hope that it could automatically tagging the information and create Trello card so that I could put the tasks into my personal planning priorization. Once I feel the workflow is running well I would have another post on that.
