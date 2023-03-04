/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","44508c759b906e67a0a1d4cc12729be0"],["/about/index.html","3b0f469e4805f4d42a6339fb3b033d5c"],["/archives/2023/01/index.html","b93df14442275975d957e321e937781c"],["/archives/2023/02/index.html","08ed5701e293515b0bde40f7ab410cae"],["/archives/2023/02/page/2/index.html","e50ecd91155102d68fbd2c9e3e9441e2"],["/archives/2023/03/index.html","e7399696940fd970565f0b3ebd9a0cfb"],["/archives/2023/index.html","e8453caff3eac752b935b188feffdc5e"],["/archives/2023/page/2/index.html","35f3ecd49d5091b54c5be2f4840e4128"],["/archives/2023/page/3/index.html","02e02f56b09b2625b438fb4ab836bb7b"],["/archives/index.html","e6b3a4f91b2e79f844d5049a1475cbb7"],["/archives/page/2/index.html","beb154cced114b295268c273bf764707"],["/archives/page/3/index.html","49c81629cea5cf01268133c52f52106d"],["/categories/Java/index.html","d35f9ba46661c329e57e6744c3db2ac8"],["/categories/Java/后端/index.html","a21fadc2ba84e2ae8e6540e452f88f7b"],["/categories/Java/基础/index.html","daa7e91db29ba5368c45d31a90de3d88"],["/categories/Java/基础/集合/index.html","4eeef22b115fde6bd90e2302fd453a64"],["/categories/Python/index.html","beedc3a142de04151fe51e1359daec11"],["/categories/Python/编程环境/index.html","083022f2482a19b2030ae214d2fcd9e3"],["/categories/R语言/index.html","abae73bbf805f8eb1bdaeb421fff3cae"],["/categories/R语言/编程环境/index.html","a563834285c364738ce9155b0be32e25"],["/categories/index.html","a5dadbd2a4a85a260f7aa62d4788c083"],["/categories/大数据开发/HBase/index.html","bc710019c092bdb1c4d01f3ba3155df8"],["/categories/大数据开发/HBase/环境搭建/index.html","6700cb0e26c4f16e760f05578d5de6fb"],["/categories/大数据开发/Hadoop/index.html","9453f09b486899caccc765e7e8a22c07"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9d544260ce9e7e958ca43937a3395c85"],["/categories/大数据开发/Zookeeper/index.html","ea06dad318e8b6bc063b7d56b33dec73"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","9a17a7618ecf7ce28a17d092503b5384"],["/categories/大数据开发/index.html","591c7364cd52a7b843c14a873cf88293"],["/categories/操作系统/Linux/index.html","72d28de1958c8185d26ee2ef4fa2b1f0"],["/categories/操作系统/Mac/index.html","679fd986fda8e799f5eb93f592836826"],["/categories/操作系统/Windows/index.html","65e8831c120d2abe5486fe80da49db8b"],["/categories/操作系统/index.html","4f335c10f55417c4a3ef550dbf978369"],["/categories/数学建模/index.html","2613e42eb0191846381b4b65cf40b77f"],["/categories/数学建模/latex/index.html","3c9d51ff1f1886bd58d71c00267afecf"],["/categories/数学建模/优化类/index.html","fe87b5335a3b2d369b8b1399f23db1b1"],["/categories/数学建模/优化类/现代优化算法/index.html","91e1d9bc1258413f9d6a226fa1eff99e"],["/categories/数学建模/优化类/规划类/index.html","9217d6373dc02ccd66b8ab93aea91218"],["/categories/数学建模/绘图/index.html","91936431e5060fa2005ef44f0951d4a5"],["/categories/数据库/MySQL/index.html","1d4e9f3f33409c45974806d69bbddabd"],["/categories/数据库/index.html","fba2ffe5fc23eda1cfd479d0ddee7a9c"],["/categories/数据结构和算法/index.html","87746aae21f3896a7a73c8e46b0f342c"],["/categories/数据结构和算法/基本原理/bfs/index.html","9fac23682bed4863dfa8b159c5e48323"],["/categories/数据结构和算法/基本原理/dfs/index.html","d0b30c57a90d96720c3608c5e6c41ae4"],["/categories/数据结构和算法/基本原理/index.html","9464a560b7df66ed7e11e073a8ef0367"],["/categories/数据结构和算法/基本原理/动态规划/index.html","ba2041da6f66d50217fb88bc3f64a737"],["/categories/数据结构和算法/基本原理/图论/index.html","e986bd469a3d0e3ff8a6756b1bfc090d"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","17d8e2d5c3199087760165ad1b16ad99"],["/categories/数据结构和算法/基本原理/数论/index.html","f7b4295b6e428e06f622186ca6c0036c"],["/categories/数据结构和算法/基本原理/树论/index.html","b0ca0a2fa0f5d76b83ad17d55f19e808"],["/categories/数据结构和算法/基本原理/链表/index.html","858f130580090ec69477d7e3045e1a35"],["/categories/数据结构和算法/算法题/index.html","a3b7fb5cfb3a0ad5667c7e1d8b3a4c1e"],["/categories/数据结构和算法/算法题/二分查找/index.html","ac40c72c1ac56743cf5a14ea5c33bbe5"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","8f22e123237fb051d6f37720ad762f4c"],["/categories/数据结构和算法/算法题/动态规划/index.html","93c3976f53849366013bc2490fe2528c"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","db10623bc6f591c2dd5ecef542dde19d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","e5d3fb305cbe89fcdbd355550eee0ed8"],["/categories/数据结构和算法/算法题/树论/index.html","cbffae81aa9f9ea626bf1ad2ad139a35"],["/categories/杂七杂八/index.html","4351ff2db5b45e57486793bb4e6de209"],["/categories/杂七杂八/博客搭建/index.html","4d255b05161cb8a99cd249328dff8a94"],["/categories/编程环境/index.html","4de7edca388e4d28868b6b4051dcf83a"],["/categories/英语学习/index.html","5108f6124e5ed0c1cd7c3db49cc15138"],["/categories/英语学习/英语语法/index.html","0bf3e8abd18f511a0df98198b94fc682"],["/comments/index.html","206b07e92a422071ee08425fb8408416"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cf82109c06f25a94d2710c54533cd37d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","272a80881c20deff80d0a42e83c982ba"],["/movies/index.html","58befd164cb6b3191adae4ea1fb5fd7a"],["/music/index.html","104b2762cc853197504d4c85dfa59d4d"],["/page/2/index.html","fa56f647eca47f0590d464018e9d3f91"],["/page/3/index.html","e8053bc5a2a5f13d781b418b5f7d5753"],["/page/4/index.html","642b09e8189e4f5ce7f70752478c5d39"],["/posts/1021360842.html","ca4b01b358a023670b859b231a7660b9"],["/posts/1120620192.html","6fc197157c91b0731a528cc118816d76"],["/posts/1141628095.html","c98bc7fc349e607ef423f6832bc2b7e4"],["/posts/1168613674.html","baa7f977461f34486db1513f752414cb"],["/posts/1219920510.html","229220054eee751b52bc6be9427f0644"],["/posts/1222166338.html","67f4ab259b93120b9d796d57353f84ac"],["/posts/1259097482.html","ac98c089fe48c650c19384bfbf2b6cb1"],["/posts/1271036369.html","91d5a08cf503ba923da4a4dd030b58ea"],["/posts/135355774.html","aed593ead5af7fb53e2cdf8347bb1f74"],["/posts/1375344716.html","37f586938c896303876e5d06d9a616a8"],["/posts/1388991698.html","9b1b6afe99efe0d9b6c1c5f332d1f480"],["/posts/1410315814.html","64740f17e7f2b51d5c8b5a8822e30046"],["/posts/1452790229.html","0776607ecff987185dac84159187e155"],["/posts/1470079884.html","d60ca4c60c16c63d0e93ff6bbc999b92"],["/posts/1470079885.html","7b253f0e21b74e684c1890cbe755cf06"],["/posts/1470079886.html","b0a7c546fdcc2f4a2fd689dc702829f9"],["/posts/1470079887.html","ea3d0cca71b94b35562e4c48644357e9"],["/posts/1498536549.html","25c6bd7909ed8327ab6f95f6133a42bc"],["/posts/1557866301.html","32f7eb7ddcb3159a271286eabe875997"],["/posts/1571776361.html","012ee9700445816c437ef8aacda9b66b"],["/posts/1605124548.html","e1ef15730cf3bfbcceddb37bff6e07c5"],["/posts/1765123828.html","dc30bccb18215f631789b10050597245"],["/posts/1817748743.html","a509a5cc9b0a6edf3510d801c0cab63e"],["/posts/1925125395.html","9107d875b2496d0830901cd673bc9bb9"],["/posts/1966191251.html","d6209ebea220222a229a1bc9101a2d8d"],["/posts/1987617322.html","68c844a5b099fdc8ef5b409559bef8af"],["/posts/2075104059.html","4263d796900891e393e22d2f17f9399a"],["/posts/2087796737.html","ab96e6dd11875751ea93f9b48449fd58"],["/posts/2207806286.html","68ca3016dee444ce5dad9af8fa27c342"],["/posts/2225903441.html","f58508e9538e4f4415862fd01137f106"],["/posts/2281352001.html","3546b95bef57da9c01382d4ae75e6014"],["/posts/2364755265.html","eae7305ae394e745a8ee0a04bef19437"],["/posts/2414116852.html","d12d918c96fd3953eea130e145fbbf1c"],["/posts/2495386210.html","26d072a4d81de370a1ebba3a2cb906a0"],["/posts/2516528882.html","66b753418e7750d1e983f629bc432598"],["/posts/2529807823.html","8d4ddbe19064d314e4201d38fee059fd"],["/posts/2888309600.html","2e437ec6a7b418ccaea91a9d5790090d"],["/posts/2891591958.html","477bd62e315ee5c4cc74d70ef58deee0"],["/posts/2909934084.html","092dd9192cab31fcd8d750cefd998872"],["/posts/3005926051.html","ba26cae846afcd98a4bf877c43d7a53e"],["/posts/3169224211.html","d02836f3604958eb235678bda6c496cf"],["/posts/3259212833.html","a65c73fedfed9d3289be25a0716ffcd6"],["/posts/3266130344.html","ec0372f567555941e27beba5ea78f1c5"],["/posts/3306641566.html","92bd0fde94635394082753d837ef19c2"],["/posts/3312011324.html","9f44880f6c1d660b604ae8a3a181d339"],["/posts/336911618.html","94ce2de077f607531496b603abfc852f"],["/posts/3402121571.html","a53005cff484cd0a0c08f54449b14696"],["/posts/3405577485.html","614c3d09b6cb249cc77d63979c7a0f67"],["/posts/3513711414.html","4655d7666d6e29889007e99737d5df59"],["/posts/3546711884.html","cee4e02f32e1d65ed3146a482a70cdef"],["/posts/3731385230.html","a609c57a1cc92a1e683eefab3216e8ab"],["/posts/3772089482.html","dcd820ab11eb81c095862de9d74f0167"],["/posts/4115971639.html","22efb541ce679b7ed9904f4e61a1ac99"],["/posts/4130790367.html","e25321391485b164f4162ca8925403c5"],["/posts/4131986683.html","d4bd957779817671cc2656dc6f9ba36f"],["/posts/4177218757.html","ff94f477c78615a985be12b9cd1620e3"],["/posts/4192183953.html","355a8b36a790a90969af7f789d61be8d"],["/posts/4261103898.html","1eba86638f88438df23c19efcbc18e4c"],["/posts/482495853.html","9bbaa7253deca8d0cb3836835edc1c3a"],["/posts/488247922.html","f14b2031da732be8cc599eac175b59a5"],["/posts/570165348.html","0530243cf93d1c928ee6a7e3f8e50e56"],["/posts/595890772.html","559c275c650216440bb7580d2c0a3512"],["/posts/694347442.html","e970090aba1bbe46f4af464a5a127b42"],["/posts/707384687.html","07fdb14df57f9d846e68d2fd809d5abc"],["/posts/71180092.html","99c5082d9763a59baed4a00d855b1489"],["/posts/716459272.html","e71abcca5c237fc0cc4c0088796596b0"],["/posts/795397410.html","c245b2cfdfabf0055748ce4e414322ae"],["/posts/820223701.html","3c4d247e25ee8d1eaf2e77362bd326c9"],["/posts/830372185.html","56fef8cd806fc5f66590c222d815c40d"],["/posts/88294277.html","28ccbf0a9c026a465afc29e23ab7685e"],["/posts/939963535.html","8b0fe59063053a12415be9956d538a34"],["/posts/983786067.html","af96614c0a727cca28577242185b810b"],["/sw-register.js","00caf7fef494221852fd6cfa9685677e"],["/tags/C/index.html","fb2ba54c9c210c369344d43e6da9ecac"],["/tags/C/page/2/index.html","d61d992ecbf43302429585b5be77095d"],["/tags/GUI/index.html","d8b0c4f809a5cb34ca2eebc3c1590dd0"],["/tags/HBase/index.html","205880e9cfe91f575fd8d4f8c14f3211"],["/tags/Hadoop/index.html","d4b3f9585eebc4adbabff8a31cc3cf1e"],["/tags/Java/index.html","ff2f22045aeeea051499288807144e8d"],["/tags/Java后端/index.html","d75563728bf3f071d72173cd69065f7d"],["/tags/Java基础/index.html","a52014dad8d4247c6094a6c8ec715db2"],["/tags/Java基础/page/2/index.html","a860d9c66c44685a2fe52dd5d93c8270"],["/tags/Linux/index.html","505bcdccab8d67dff3d2bd37d92ff97a"],["/tags/Linux/page/2/index.html","6e0151ac99e68764113ce15e4ded70cf"],["/tags/Mac/index.html","ca7bce5fc39702e0c4400678fb7cb9a5"],["/tags/Mac/page/2/index.html","8ece84e416c197ae7b10f867afded33b"],["/tags/Maven/index.html","0c97f6514ccd501eb8f0e464a0a8b87f"],["/tags/MySQL/index.html","e936e82cf27cc86083902b5d4b2fb2a2"],["/tags/Python/index.html","a78d559ac4c59d9ef6c6a7364436e68f"],["/tags/R语言/index.html","9899cd0077b766c333f43c7d1b70fcbc"],["/tags/Ubuntu/index.html","1f4d5d3dea855799bfd98dd0b25d6da6"],["/tags/Windows/index.html","b655670e9fc9bda524549f088a83d63a"],["/tags/Zookeeper/index.html","23eae4188d42390ceb3f644ede947ef0"],["/tags/bfs/index.html","4409abd5d66ca21ecc8eae0bd5ac7269"],["/tags/dfs/index.html","b54b55f22beccddc69affdac523ccccf"],["/tags/folium/index.html","c461e1a54d8c6afbc0dc3dce496cff55"],["/tags/git/index.html","0e4fe45d5bb03c19495b6ed4f3371a0f"],["/tags/index.html","8268f468637b512687827d5b23f5acea"],["/tags/latex/index.html","0f347ca38d5ef098716691e1bac0800e"],["/tags/二分查找/index.html","6b9322ee3dcd75070ff901b716bcc8e2"],["/tags/优化类/index.html","a4d6cc62323c613de9b5ae071adb72e8"],["/tags/前缀和与差分/index.html","175a6f7b48c14f83f8918ffac5e06502"],["/tags/动态规划/index.html","edccea036ac2f7d06f7c73aa0e4e3186"],["/tags/博客搭建/index.html","14e5dfd3e66c3c09841a00edbfb4e0ea"],["/tags/图论/index.html","d0b3ba9f79893c982a00b349c6c89d03"],["/tags/大数据/index.html","52cf3b3eb5c9f0b97032f1cb6d545336"],["/tags/操作系统/index.html","9824775e7cd2e71527c99fb471e2bbd8"],["/tags/数学建模/index.html","402bdaa6dfcec22c988b452885c2da23"],["/tags/数据库/index.html","604e1dcf4491fc382da4fbf3b753c544"],["/tags/数据结构和算法/index.html","fe5fd0048a8874f70d7f162e8722bda6"],["/tags/数据结构和算法/page/2/index.html","6e564f74159fbf9f4f85c2f05268948e"],["/tags/数组和字符串/index.html","1346e888cde5324fafd3a49188fec1db"],["/tags/枚举类/index.html","ab9a501933df643671e226806a384f1b"],["/tags/栈和队列/index.html","66bd49eef9053c152c52caaedf811542"],["/tags/树论/index.html","7aee8308fc35267810e89b530fcc1208"],["/tags/测试/index.html","a21b8057975142ef59c0a63ced062db1"],["/tags/环境/index.html","797f03fbc215406968b5d8ed65139176"],["/tags/环境变量/index.html","a3f6e64b5d79464ee1d53ee8c12de139"],["/tags/绘图/index.html","5da22684a2a58048659a2a755db8cd1e"],["/tags/编程环境/index.html","774826f6cb83355f23d9dbb7d2a12fc1"],["/tags/网络编程/index.html","ae479f630976fd02ff4a2a0211e4bd20"],["/tags/英语语法/index.html","3698600cf1939f73a53d46c69875454d"],["/tags/论文/index.html","9667d93f1fa89e027ef2c0bfa234206d"],["/tags/资源下载/index.html","e398ea2a1c4198d15f09abf793b3fceb"],["/tags/链表/index.html","51a8258d37518025c05933f216e4f35f"],["/tags/集合/index.html","2bb6c05092784528627e757defa71c48"],["/tags/集群/index.html","16cfda701eecc4df5c083b1244fc6ad9"]];
var cacheName = 'sw-precache-v3--' + (self.registration ? self.registration.scope : '');
var firstRegister = 1; // 默认1是首次安装SW， 0是SW更新


var ignoreUrlParametersMatching = [/^utm_/];


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var cleanResponse = function (originalResponse) {
    // 如果没有重定向响应，不需干啥
    if (!originalResponse.redirected) {
        return Promise.resolve(originalResponse);
    }

    // Firefox 50 及以下不知处 Response.body 流, 所以我们需要读取整个body以blob形式返回。
    var bodyPromise = 'body' in originalResponse ?
        Promise.resolve(originalResponse.body) :
        originalResponse.blob();

    return bodyPromise.then(function (body) {
        // new Response() 可同时支持 stream or Blob.
        return new Response(body, {
            headers: originalResponse.headers,
            status: originalResponse.status,
            statusText: originalResponse.statusText
        });
    });
};

var createCacheKey = function (originalUrl, paramName, paramValue,
    dontCacheBustUrlsMatching) {

    // 创建一个新的URL对象，避免影响原始URL
    var url = new URL(originalUrl);

    // 如果 dontCacheBustUrlsMatching 值没有设置，或是没有匹配到，将值拼接到url.serach后
    if (!dontCacheBustUrlsMatching ||
        !(url.pathname.match(dontCacheBustUrlsMatching))) {
        url.search += (url.search ? '&' : '') +
            encodeURIComponent(paramName) + '=' + encodeURIComponent(paramValue);
    }

    return url.toString();
};

var isPathWhitelisted = function (whitelist, absoluteUrlString) {
    // 如果 whitelist 是空数组，则认为全部都在白名单内
    if (whitelist.length === 0) {
        return true;
    }

    // 否则逐个匹配正则匹配并返回
    var path = (new URL(absoluteUrlString)).pathname;
    return whitelist.some(function (whitelistedPathRegex) {
        return path.match(whitelistedPathRegex);
    });
};

var stripIgnoredUrlParameters = function (originalUrl,
    ignoreUrlParametersMatching) {
    var url = new URL(originalUrl);
    // 移除 hash; 查看 https://github.com/GoogleChrome/sw-precache/issues/290
    url.hash = '';

    url.search = url.search.slice(1) // 是否包含 '?'
        .split('&') // 分割成数组 'key=value' 的形式
        .map(function (kv) {
            return kv.split('='); // 分割每个 'key=value' 字符串成 [key, value] 形式
        })
        .filter(function (kv) {
            return ignoreUrlParametersMatching.every(function (ignoredRegex) {
                return !ignoredRegex.test(kv[0]); // 如果 key 没有匹配到任何忽略参数正则，就 Return true
            });
        })
        .map(function (kv) {
            return kv.join('='); // 重新把 [key, value] 格式转换为 'key=value' 字符串
        })
        .join('&'); // 将所有参数 'key=value' 以 '&' 拼接

    return url.toString();
};


var addDirectoryIndex = function (originalUrl, index) {
    var url = new URL(originalUrl);
    if (url.pathname.slice(-1) === '/') {
        url.pathname += index;
    }
    return url.toString();
};

var hashParamName = '_sw-precache';
var urlsToCacheKeys = new Map(
    precacheConfig.map(function (item) {
        var relativeUrl = item[0];
        var hash = item[1];
        var absoluteUrl = new URL(relativeUrl, self.location);
        var cacheKey = createCacheKey(absoluteUrl, hashParamName, hash, false);
        return [absoluteUrl.toString(), cacheKey];
    })
);

function setOfCachedUrls(cache) {
    return cache.keys().then(function (requests) {
        // 如果原cacheName中没有缓存任何收，就默认是首次安装，否则认为是SW更新
        if (requests && requests.length > 0) {
            firstRegister = 0; // SW更新
        }
        return requests.map(function (request) {
            return request.url;
        });
    }).then(function (urls) {
        return new Set(urls);
    });
}

self.addEventListener('install', function (event) {
    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return setOfCachedUrls(cache).then(function (cachedUrls) {
                return Promise.all(
                    Array.from(urlsToCacheKeys.values()).map(function (cacheKey) {
                        // 如果缓存中没有匹配到cacheKey，添加进去
                        if (!cachedUrls.has(cacheKey)) {
                            var request = new Request(cacheKey, { credentials: 'same-origin' });
                            return fetch(request).then(function (response) {
                                // 只要返回200才能继续，否则直接抛错
                                if (!response.ok) {
                                    throw new Error('Request for ' + cacheKey + ' returned a ' +
                                        'response with status ' + response.status);
                                }

                                return cleanResponse(response).then(function (responseToCache) {
                                    return cache.put(cacheKey, responseToCache);
                                });
                            });
                        }
                    })
                );
            });
        })
            .then(function () {
            
            // 强制 SW 状态 installing -> activate
            return self.skipWaiting();
            
        })
    );
});

self.addEventListener('activate', function (event) {
    var setOfExpectedUrls = new Set(urlsToCacheKeys.values());

    event.waitUntil(
        caches.open(cacheName).then(function (cache) {
            return cache.keys().then(function (existingRequests) {
                return Promise.all(
                    existingRequests.map(function (existingRequest) {
                        // 删除原缓存中相同键值内容
                        if (!setOfExpectedUrls.has(existingRequest.url)) {
                            return cache.delete(existingRequest);
                        }
                    })
                );
            });
        }).then(function () {
            
            return self.clients.claim();
            
        }).then(function () {
                // 如果是首次安装 SW 时, 不发送更新消息（是否是首次安装，通过指定cacheName 中是否有缓存信息判断）
                // 如果不是首次安装，则是内容有更新，需要通知页面重载更新
                if (!firstRegister) {
                    return self.clients.matchAll()
                        .then(function (clients) {
                            if (clients && clients.length) {
                                clients.forEach(function (client) {
                                    client.postMessage('sw.update');
                                })
                            }
                        })
                }
            })
    );
});



    self.addEventListener('fetch', function (event) {
        if (event.request.method === 'GET') {

            // 是否应该 event.respondWith()，需要我们逐步的判断
            // 而且也方便了后期做特殊的特殊
            var shouldRespond;


            // 首先去除已配置的忽略参数及hash
            // 查看缓存简直中是否包含该请求，包含就将shouldRespond 设为true
            var url = stripIgnoredUrlParameters(event.request.url, ignoreUrlParametersMatching);
            shouldRespond = urlsToCacheKeys.has(url);

            // 如果 shouldRespond 是 false, 我们在url后默认增加 'index.html'
            // (或者是你在配置文件中自行配置的 directoryIndex 参数值)，继续查找缓存列表
            var directoryIndex = 'index.html';
            if (!shouldRespond && directoryIndex) {
                url = addDirectoryIndex(url, directoryIndex);
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 仍是 false，检查是否是navigation
            // request， 如果是的话，判断是否能与 navigateFallbackWhitelist 正则列表匹配
            var navigateFallback = '';
            if (!shouldRespond &&
                navigateFallback &&
                (event.request.mode === 'navigate') &&
                isPathWhitelisted([], event.request.url)
            ) {
                url = new URL(navigateFallback, self.location).toString();
                shouldRespond = urlsToCacheKeys.has(url);
            }

            // 如果 shouldRespond 被置为 true
            // 则 event.respondWith()匹配缓存返回结果，匹配不成就直接请求.
            if (shouldRespond) {
                event.respondWith(
                    caches.open(cacheName).then(function (cache) {
                        return cache.match(urlsToCacheKeys.get(url)).then(function (response) {
                            if (response) {
                                return response;
                            }
                            throw Error('The cached response that was expected is missing.');
                        });
                    }).catch(function (e) {
                        // 如果捕获到异常错误，直接返回 fetch() 请求资源
                        console.warn('Couldn\'t serve response for "%s" from cache: %O', event.request.url, e);
                        return fetch(event.request);
                    })
                );
            }
        }
    });



// *** Start of auto-included sw-toolbox code. ***
/* 
 Copyright 2016 Google Inc. All Rights Reserved.

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/!function(e){if("object"==typeof exports&&"undefined"!=typeof module)module.exports=e();else if("function"==typeof define&&define.amd)define([],e);else{var t;t="undefined"!=typeof window?window:"undefined"!=typeof global?global:"undefined"!=typeof self?self:this,t.toolbox=e()}}(function(){return function e(t,n,r){function o(c,s){if(!n[c]){if(!t[c]){var a="function"==typeof require&&require;if(!s&&a)return a(c,!0);if(i)return i(c,!0);var u=new Error("Cannot find module '"+c+"'");throw u.code="MODULE_NOT_FOUND",u}var f=n[c]={exports:{}};t[c][0].call(f.exports,function(e){var n=t[c][1][e];return o(n?n:e)},f,f.exports,e,t,n,r)}return n[c].exports}for(var i="function"==typeof require&&require,c=0;c<r.length;c++)o(r[c]);return o}({1:[function(e,t,n){"use strict";function r(e,t){t=t||{};var n=t.debug||m.debug;n&&console.log("[sw-toolbox] "+e)}function o(e){var t;return e&&e.cache&&(t=e.cache.name),t=t||m.cache.name,caches.open(t)}function i(e,t){t=t||{};var n=t.successResponses||m.successResponses;return fetch(e.clone()).then(function(r){return"GET"===e.method&&n.test(r.status)&&o(t).then(function(n){n.put(e,r).then(function(){var r=t.cache||m.cache;(r.maxEntries||r.maxAgeSeconds)&&r.name&&c(e,n,r)})}),r.clone()})}function c(e,t,n){var r=s.bind(null,e,t,n);d=d?d.then(r):r()}function s(e,t,n){var o=e.url,i=n.maxAgeSeconds,c=n.maxEntries,s=n.name,a=Date.now();return r("Updating LRU order for "+o+". Max entries is "+c+", max age is "+i),g.getDb(s).then(function(e){return g.setTimestampForUrl(e,o,a)}).then(function(e){return g.expireEntries(e,c,i,a)}).then(function(e){r("Successfully updated IDB.");var n=e.map(function(e){return t.delete(e)});return Promise.all(n).then(function(){r("Done with cache cleanup.")})}).catch(function(e){r(e)})}function a(e,t,n){return r("Renaming cache: ["+e+"] to ["+t+"]",n),caches.delete(t).then(function(){return Promise.all([caches.open(e),caches.open(t)]).then(function(t){var n=t[0],r=t[1];return n.keys().then(function(e){return Promise.all(e.map(function(e){return n.match(e).then(function(t){return r.put(e,t)})}))}).then(function(){return caches.delete(e)})})})}function u(e,t){return o(t).then(function(t){return t.add(e)})}function f(e,t){return o(t).then(function(t){return t.delete(e)})}function h(e){e instanceof Promise||p(e),m.preCacheItems=m.preCacheItems.concat(e)}function p(e){var t=Array.isArray(e);if(t&&e.forEach(function(e){"string"==typeof e||e instanceof Request||(t=!1)}),!t)throw new TypeError("The precache method expects either an array of strings and/or Requests or a Promise that resolves to an array of strings and/or Requests.");return e}function l(e,t,n){if(!e)return!1;if(t){var r=e.headers.get("date");if(r){var o=new Date(r);if(o.getTime()+1e3*t<n)return!1}}return!0}var d,m=e("./options"),g=e("./idb-cache-expiration");t.exports={debug:r,fetchAndCache:i,openCache:o,renameCache:a,cache:u,uncache:f,precache:h,validatePrecacheInput:p,isResponseFresh:l}},{"./idb-cache-expiration":2,"./options":4}],2:[function(e,t,n){"use strict";function r(e){return new Promise(function(t,n){var r=indexedDB.open(u+e,f);r.onupgradeneeded=function(){var e=r.result.createObjectStore(h,{keyPath:p});e.createIndex(l,l,{unique:!1})},r.onsuccess=function(){t(r.result)},r.onerror=function(){n(r.error)}})}function o(e){return e in d||(d[e]=r(e)),d[e]}function i(e,t,n){return new Promise(function(r,o){var i=e.transaction(h,"readwrite"),c=i.objectStore(h);c.put({url:t,timestamp:n}),i.oncomplete=function(){r(e)},i.onabort=function(){o(i.error)}})}function c(e,t,n){return t?new Promise(function(r,o){var i=1e3*t,c=[],s=e.transaction(h,"readwrite"),a=s.objectStore(h),u=a.index(l);u.openCursor().onsuccess=function(e){var t=e.target.result;if(t&&n-i>t.value[l]){var r=t.value[p];c.push(r),a.delete(r),t.continue()}},s.oncomplete=function(){r(c)},s.onabort=o}):Promise.resolve([])}function s(e,t){return t?new Promise(function(n,r){var o=[],i=e.transaction(h,"readwrite"),c=i.objectStore(h),s=c.index(l),a=s.count();s.count().onsuccess=function(){var e=a.result;e>t&&(s.openCursor().onsuccess=function(n){var r=n.target.result;if(r){var i=r.value[p];o.push(i),c.delete(i),e-o.length>t&&r.continue()}})},i.oncomplete=function(){n(o)},i.onabort=r}):Promise.resolve([])}function a(e,t,n,r){return c(e,n,r).then(function(n){return s(e,t).then(function(e){return n.concat(e)})})}var u="sw-toolbox-",f=1,h="store",p="url",l="timestamp",d={};t.exports={getDb:o,setTimestampForUrl:i,expireEntries:a}},{}],3:[function(e,t,n){"use strict";function r(e){var t=a.match(e.request);t?e.respondWith(t(e.request)):a.default&&"GET"===e.request.method&&0===e.request.url.indexOf("http")&&e.respondWith(a.default(e.request))}function o(e){s.debug("activate event fired");var t=u.cache.name+"$$$inactive$$$";e.waitUntil(s.renameCache(t,u.cache.name))}function i(e){return e.reduce(function(e,t){return e.concat(t)},[])}function c(e){var t=u.cache.name+"$$$inactive$$$";s.debug("install event fired"),s.debug("creating cache ["+t+"]"),e.waitUntil(s.openCache({cache:{name:t}}).then(function(e){return Promise.all(u.preCacheItems).then(i).then(s.validatePrecacheInput).then(function(t){return s.debug("preCache list: "+(t.join(", ")||"(none)")),e.addAll(t)})}))}e("serviceworker-cache-polyfill");var s=e("./helpers"),a=e("./router"),u=e("./options");t.exports={fetchListener:r,activateListener:o,installListener:c}},{"./helpers":1,"./options":4,"./router":6,"serviceworker-cache-polyfill":16}],4:[function(e,t,n){"use strict";var r;r=self.registration?self.registration.scope:self.scope||new URL("./",self.location).href,t.exports={cache:{name:"$$$toolbox-cache$$$"+r+"$$$",maxAgeSeconds:null,maxEntries:null},debug:!1,networkTimeoutSeconds:null,preCacheItems:[],successResponses:/^0|([123]\d\d)|(40[14567])|410$/}},{}],5:[function(e,t,n){"use strict";var r=new URL("./",self.location),o=r.pathname,i=e("path-to-regexp"),c=function(e,t,n,r){t instanceof RegExp?this.fullUrlRegExp=t:(0!==t.indexOf("/")&&(t=o+t),this.keys=[],this.regexp=i(t,this.keys)),this.method=e,this.options=r,this.handler=n};c.prototype.makeHandler=function(e){var t;if(this.regexp){var n=this.regexp.exec(e);t={},this.keys.forEach(function(e,r){t[e.name]=n[r+1]})}return function(e){return this.handler(e,t,this.options)}.bind(this)},t.exports=c},{"path-to-regexp":15}],6:[function(e,t,n){"use strict";function r(e){return e.replace(/[-\/\\^$*+?.()|[\]{}]/g,"\\$&")}var o=e("./route"),i=e("./helpers"),c=function(e,t){for(var n=e.entries(),r=n.next(),o=[];!r.done;){var i=new RegExp(r.value[0]);i.test(t)&&o.push(r.value[1]),r=n.next()}return o},s=function(){this.routes=new Map,this.routes.set(RegExp,new Map),this.default=null};["get","post","put","delete","head","any"].forEach(function(e){s.prototype[e]=function(t,n,r){return this.add(e,t,n,r)}}),s.prototype.add=function(e,t,n,c){c=c||{};var s;t instanceof RegExp?s=RegExp:(s=c.origin||self.location.origin,s=s instanceof RegExp?s.source:r(s)),e=e.toLowerCase();var a=new o(e,t,n,c);this.routes.has(s)||this.routes.set(s,new Map);var u=this.routes.get(s);u.has(e)||u.set(e,new Map);var f=u.get(e),h=a.regexp||a.fullUrlRegExp;f.has(h.source)&&i.debug('"'+t+'" resolves to same regex as existing route.'),f.set(h.source,a)},s.prototype.matchMethod=function(e,t){var n=new URL(t),r=n.origin,o=n.pathname;return this._match(e,c(this.routes,r),o)||this._match(e,[this.routes.get(RegExp)],t)},s.prototype._match=function(e,t,n){if(0===t.length)return null;for(var r=0;r<t.length;r++){var o=t[r],i=o&&o.get(e.toLowerCase());if(i){var s=c(i,n);if(s.length>0)return s[0].makeHandler(n)}}return null},s.prototype.match=function(e){return this.matchMethod(e.method,e.url)||this.matchMethod("any",e.url)},t.exports=new s},{"./helpers":1,"./route":5}],7:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache first ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(t){var r=n.cache||o.cache,c=Date.now();return i.isResponseFresh(t,r.maxAgeSeconds,c)?t:i.fetchAndCache(e,n)})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],8:[function(e,t,n){"use strict";function r(e,t,n){return n=n||{},i.debug("Strategy: cache only ["+e.url+"]",n),i.openCache(n).then(function(t){return t.match(e).then(function(e){var t=n.cache||o.cache,r=Date.now();if(i.isResponseFresh(e,t.maxAgeSeconds,r))return e})})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],9:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: fastest ["+e.url+"]",n),new Promise(function(r,c){var s=!1,a=[],u=function(e){a.push(e.toString()),s?c(new Error('Both cache and network failed: "'+a.join('", "')+'"')):s=!0},f=function(e){e instanceof Response?r(e):u("No result returned")};o.fetchAndCache(e.clone(),n).then(f,u),i(e,t,n).then(f,u)})}var o=e("../helpers"),i=e("./cacheOnly");t.exports=r},{"../helpers":1,"./cacheOnly":8}],10:[function(e,t,n){t.exports={networkOnly:e("./networkOnly"),networkFirst:e("./networkFirst"),cacheOnly:e("./cacheOnly"),cacheFirst:e("./cacheFirst"),fastest:e("./fastest")}},{"./cacheFirst":7,"./cacheOnly":8,"./fastest":9,"./networkFirst":11,"./networkOnly":12}],11:[function(e,t,n){"use strict";function r(e,t,n){n=n||{};var r=n.successResponses||o.successResponses,c=n.networkTimeoutSeconds||o.networkTimeoutSeconds;return i.debug("Strategy: network first ["+e.url+"]",n),i.openCache(n).then(function(t){var s,a,u=[];if(c){var f=new Promise(function(r){s=setTimeout(function(){t.match(e).then(function(e){var t=n.cache||o.cache,c=Date.now(),s=t.maxAgeSeconds;i.isResponseFresh(e,s,c)&&r(e)})},1e3*c)});u.push(f)}var h=i.fetchAndCache(e,n).then(function(e){if(s&&clearTimeout(s),r.test(e.status))return e;throw i.debug("Response was an HTTP error: "+e.statusText,n),a=e,new Error("Bad response")}).catch(function(r){return i.debug("Network or response error, fallback to cache ["+e.url+"]",n),t.match(e).then(function(e){if(e)return e;if(a)return a;throw r})});return u.push(h),Promise.race(u)})}var o=e("../options"),i=e("../helpers");t.exports=r},{"../helpers":1,"../options":4}],12:[function(e,t,n){"use strict";function r(e,t,n){return o.debug("Strategy: network only ["+e.url+"]",n),fetch(e)}var o=e("../helpers");t.exports=r},{"../helpers":1}],13:[function(e,t,n){"use strict";var r=e("./options"),o=e("./router"),i=e("./helpers"),c=e("./strategies"),s=e("./listeners");i.debug("Service Worker Toolbox is loading"),self.addEventListener("install",s.installListener),self.addEventListener("activate",s.activateListener),self.addEventListener("fetch",s.fetchListener),t.exports={networkOnly:c.networkOnly,networkFirst:c.networkFirst,cacheOnly:c.cacheOnly,cacheFirst:c.cacheFirst,fastest:c.fastest,router:o,options:r,cache:i.cache,uncache:i.uncache,precache:i.precache}},{"./helpers":1,"./listeners":3,"./options":4,"./router":6,"./strategies":10}],14:[function(e,t,n){t.exports=Array.isArray||function(e){return"[object Array]"==Object.prototype.toString.call(e)}},{}],15:[function(e,t,n){function r(e,t){for(var n,r=[],o=0,i=0,c="",s=t&&t.delimiter||"/";null!=(n=x.exec(e));){var f=n[0],h=n[1],p=n.index;if(c+=e.slice(i,p),i=p+f.length,h)c+=h[1];else{var l=e[i],d=n[2],m=n[3],g=n[4],v=n[5],w=n[6],y=n[7];c&&(r.push(c),c="");var b=null!=d&&null!=l&&l!==d,E="+"===w||"*"===w,R="?"===w||"*"===w,k=n[2]||s,$=g||v;r.push({name:m||o++,prefix:d||"",delimiter:k,optional:R,repeat:E,partial:b,asterisk:!!y,pattern:$?u($):y?".*":"[^"+a(k)+"]+?"})}}return i<e.length&&(c+=e.substr(i)),c&&r.push(c),r}function o(e,t){return s(r(e,t))}function i(e){return encodeURI(e).replace(/[\/?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function c(e){return encodeURI(e).replace(/[?#]/g,function(e){return"%"+e.charCodeAt(0).toString(16).toUpperCase()})}function s(e){for(var t=new Array(e.length),n=0;n<e.length;n++)"object"==typeof e[n]&&(t[n]=new RegExp("^(?:"+e[n].pattern+")$"));return function(n,r){for(var o="",s=n||{},a=r||{},u=a.pretty?i:encodeURIComponent,f=0;f<e.length;f++){var h=e[f];if("string"!=typeof h){var p,l=s[h.name];if(null==l){if(h.optional){h.partial&&(o+=h.prefix);continue}throw new TypeError('Expected "'+h.name+'" to be defined')}if(v(l)){if(!h.repeat)throw new TypeError('Expected "'+h.name+'" to not repeat, but received `'+JSON.stringify(l)+"`");if(0===l.length){if(h.optional)continue;throw new TypeError('Expected "'+h.name+'" to not be empty')}for(var d=0;d<l.length;d++){if(p=u(l[d]),!t[f].test(p))throw new TypeError('Expected all "'+h.name+'" to match "'+h.pattern+'", but received `'+JSON.stringify(p)+"`");o+=(0===d?h.prefix:h.delimiter)+p}}else{if(p=h.asterisk?c(l):u(l),!t[f].test(p))throw new TypeError('Expected "'+h.name+'" to match "'+h.pattern+'", but received "'+p+'"');o+=h.prefix+p}}else o+=h}return o}}function a(e){return e.replace(/([.+*?=^!:${}()[\]|\/\\])/g,"\\$1")}function u(e){return e.replace(/([=!:$\/()])/g,"\\$1")}function f(e,t){return e.keys=t,e}function h(e){return e.sensitive?"":"i"}function p(e,t){var n=e.source.match(/\((?!\?)/g);if(n)for(var r=0;r<n.length;r++)t.push({name:r,prefix:null,delimiter:null,optional:!1,repeat:!1,partial:!1,asterisk:!1,pattern:null});return f(e,t)}function l(e,t,n){for(var r=[],o=0;o<e.length;o++)r.push(g(e[o],t,n).source);var i=new RegExp("(?:"+r.join("|")+")",h(n));return f(i,t)}function d(e,t,n){return m(r(e,n),t,n)}function m(e,t,n){v(t)||(n=t||n,t=[]),n=n||{};for(var r=n.strict,o=n.end!==!1,i="",c=0;c<e.length;c++){var s=e[c];if("string"==typeof s)i+=a(s);else{var u=a(s.prefix),p="(?:"+s.pattern+")";t.push(s),s.repeat&&(p+="(?:"+u+p+")*"),p=s.optional?s.partial?u+"("+p+")?":"(?:"+u+"("+p+"))?":u+"("+p+")",i+=p}}var l=a(n.delimiter||"/"),d=i.slice(-l.length)===l;return r||(i=(d?i.slice(0,-l.length):i)+"(?:"+l+"(?=$))?"),i+=o?"$":r&&d?"":"(?="+l+"|$)",f(new RegExp("^"+i,h(n)),t)}function g(e,t,n){return v(t)||(n=t||n,t=[]),n=n||{},e instanceof RegExp?p(e,t):v(e)?l(e,t,n):d(e,t,n)}var v=e("isarray");t.exports=g,t.exports.parse=r,t.exports.compile=o,t.exports.tokensToFunction=s,t.exports.tokensToRegExp=m;var x=new RegExp(["(\\\\.)","([\\/.])?(?:(?:\\:(\\w+)(?:\\(((?:\\\\.|[^\\\\()])+)\\))?|\\(((?:\\\\.|[^\\\\()])+)\\))([+*?])?|(\\*))"].join("|"),"g")},{isarray:14}],16:[function(e,t,n){!function(){var e=Cache.prototype.addAll,t=navigator.userAgent.match(/(Firefox|Chrome)\/(\d+\.)/);if(t)var n=t[1],r=parseInt(t[2]);e&&(!t||"Firefox"===n&&r>=46||"Chrome"===n&&r>=50)||(Cache.prototype.addAll=function(e){function t(e){this.name="NetworkError",this.code=19,this.message=e}var n=this;return t.prototype=Object.create(Error.prototype),Promise.resolve().then(function(){if(arguments.length<1)throw new TypeError;return e=e.map(function(e){return e instanceof Request?e:String(e)}),Promise.all(e.map(function(e){"string"==typeof e&&(e=new Request(e));var n=new URL(e.url).protocol;if("http:"!==n&&"https:"!==n)throw new t("Invalid scheme");return fetch(e.clone())}))}).then(function(r){if(r.some(function(e){return!e.ok}))throw new t("Incorrect response status");return Promise.all(r.map(function(t,r){return n.put(e[r],t)}))}).then(function(){})},Cache.prototype.add=function(e){return this.addAll([e])})}()},{}]},{},[13])(13)});


// *** End of auto-included sw-toolbox code. ***



// Runtime cache 配置转换后的 toolbox 代码.

toolbox.router.get("/*", toolbox.cacheFirst, {"origin":"unpkg.com"});
toolbox.router.get("/npm/*", toolbox.cacheFirst, {"origin":"cdn.jsdelivr.net"});





/* eslint-enable */
