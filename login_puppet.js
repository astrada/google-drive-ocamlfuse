const puppeteer = require("puppeteer");

/* 

  Author: Peter Herz (peter at tachy0n.com)
  
  Login to Google Drive automatically for google-drive-ocamlfuse
  Add user, pass, and clientID ("appid") to get_auth_code.sh and run it
  to invoke this puppet script. To debug you may need to set head to false (not headless)
  and use Chromium dev tools to figure out what's going wrong. 
  
  Note: From tests had subtly different behaviors on a local Chromium session on MacOS 
  compared to when run on a Fedora VPS box without ability to run non-headless.

*/

const debug = false;
const  head = true;

const  argv = process.argv;
const  user = argv[2];
const  pass = argv[3];
const  clid = argv[4];

(async () => {
  const browser = await puppeteer.launch({devtools: true, slowMo: 100, headless: head, args: ['--no-sandbox']});
  const page = await browser.newPage();
  await page.setCacheEnabled(false);
  await page.goto("https://accounts.google.com/o/oauth2/auth?client_id="+clid+"&redirect_uri=urn%3Aietf%3Awg%3Aoauth%3A2.0%3Aoob&scope=https%3A%2F%2Fwww.googleapis.com%2Fauth%2Fdrive&response_type=code&access_type=offline&approval_prompt=force", {waitUntil: 'load'});
  await page.on('console', msg => console.log('PAGE LOG:', msg.text()));

  const navigationPromise = page.waitForNavigation();
  await page.waitForSelector('input[type="email"]', { visible: true, timeout: 10000 }).then( (debug) ? console.log('found login') : false );
  await page.type('input[type="email"]', user);
  await page.type('input[type="email"]', String.fromCharCode(13));
  await page.waitForSelector('input[type="password"]', { visible: true, timeout: 10000 }).then( (debug) ? console.log('found pw') : false );
  await page.type('input[type="password"]', pass);
  await page.type('input[type="password"]', String.fromCharCode(13));

  await page.waitForSelector('a', { visible: true, timeout: 10000 }).then( async e => {
    setTimeout( function() {
      (debug) ? console.log('test 1') : false;
      const clickByText = async function(page, text, element) {
        await page.evaluate(() => {
          const xp = document.evaluate("(//a[contains(text(), 'unsafe')])[1]", document);
             document.getElementById('submit_approve_access').click();
        });
      };
      clickByText(page, 'Go to pherzohlc (unsafe)');
    }, 1000)


  }); // end of await selector

  await page.waitForSelector('div', { visible: true, timeout: 10000 }).then( async e => {

    setTimeout( function() {
       page.evaluate(() => {  document.getElementById('submit_approve_access'); });
    }, 1800)
  }); // end of await selector

  var code;
  await page.waitForSelector('textarea', { visible: true, timeout: 10000 }).then( async e => {
    setTimeout( function() {
      (debug) ? console.log('test 4') : false;
      const clickByText = async function(page, text, element) {
        const getData = async() => {
          return await page.evaluate(async () => {
              return await new Promise(resolve => {
                resolve(document.querySelectorAll('textarea')[0].value)
            })
          })
        }
        code = await getData();
        console.log(code);
        await browser.close();
      };
      clickByText(page, 'Allow');
    }, 1200)
  }); // end of await selector
})();#
