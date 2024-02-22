/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","348ec6e64f32d26586a5f5b1b88b2175"],["/about/index.html","30a13bdcc2291731ee6947dd68ac76fd"],["/archives/2023/01/index.html","8d3690999d288555d2a59d42b8504af9"],["/archives/2023/02/index.html","7975f2ed98bb1d89d388470b3eb7574a"],["/archives/2023/02/page/2/index.html","b5e0813004329c9711f18a084a8f9d94"],["/archives/2023/03/index.html","59b32d08acfefaf1c0c9ddfd6e583cb0"],["/archives/2023/05/index.html","542f321d66e684d0cecdd3a2be30edab"],["/archives/2023/06/index.html","5e6d8e940c766206c799bd2f865c6a55"],["/archives/2023/09/index.html","9ab268381c521dc7245326edd15353be"],["/archives/2023/11/index.html","06fc314f1fec8fd3dcbea7000663e01f"],["/archives/2023/12/index.html","815c43c860c9fee3610eb6496216bc1b"],["/archives/2023/index.html","c3c441c805d43b1e12d3d09084cfa30b"],["/archives/2023/page/2/index.html","2f5d112c12254b361a7f1851de4f7025"],["/archives/2023/page/3/index.html","b969622ae81cda74b67a4a9dd7557f08"],["/archives/2023/page/4/index.html","c8082bb10a8473f4713f5fe8b1af8ddf"],["/archives/2024/02/index.html","a3921850021dcb8962886ec27cb6663a"],["/archives/2024/index.html","d9a9c78000a47c2be7a949bf25d88fbb"],["/archives/index.html","20f28efd504a839232e3be95b73bc004"],["/archives/page/2/index.html","7562b0a8eb39acff947054c466bd7dfe"],["/archives/page/3/index.html","ea73258e00b68f29c9177f0012c5a0b5"],["/archives/page/4/index.html","4aeeafc44b785591acf5633b6fd2914c"],["/baidu_verify_codeva-qQP2iZOMLX.html","7c9ec2611790013789794d54b18545a4"],["/categories/Java/index.html","79e37b1d928999122eb5bd5db008d5d1"],["/categories/Java/后端/index.html","7a5cb69e1973ff12ab88a396ad75a1bf"],["/categories/Java/基础/index.html","075de9e45d776369bb8477c3319b9ce2"],["/categories/Java/基础/集合/index.html","4cfad16c172ccdc1a1ffd97b4532945a"],["/categories/Python/index.html","314567183185bba4d425bc2bb6f9f305"],["/categories/Python/编程环境/index.html","90afac27fa82b268248317a195c176b4"],["/categories/R语言/index.html","425f205e182d89126e5a72e4e6c7f908"],["/categories/R语言/编程环境/index.html","fbdc7ad88e3e0d6b89d469db1869bd06"],["/categories/index.html","63835c7e5e6487e39c222a58200244b5"],["/categories/中间件/index.html","27672dfe9860448c0bfdeef66c491700"],["/categories/前端/Vue/index.html","655fedc27c62cc8b1af68c4e25ac0cde"],["/categories/前端/index.html","45381c058009e22bcb30ce2eeccb9c93"],["/categories/大数据开发/ElasticSearch/index.html","5f22d0467b11c7aaed1e57592e77796e"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7edf4a005a454ff4415906a92aaa29a9"],["/categories/大数据开发/HBase/index.html","e0e965137cbe7cec9c20d46bff0209dd"],["/categories/大数据开发/HBase/学习笔记/index.html","c4348f92feec2cabb375650836281b73"],["/categories/大数据开发/HBase/环境搭建/index.html","4d32e3c64be449fad02b013b43a73848"],["/categories/大数据开发/Hadoop/index.html","46ec22f4032f70b7f019b805ddc4bfe4"],["/categories/大数据开发/Hadoop/技术/index.html","ffec887551354767307c799aed35d9e2"],["/categories/大数据开发/Hadoop/环境搭建/index.html","a1f24554d4a1c9152b2f87824e961b24"],["/categories/大数据开发/Redis/index.html","39c4fee229e65838a245fdede897bb1b"],["/categories/大数据开发/Redis/技术/index.html","9762551cdcb7a4f9b7146e4c1c8efe03"],["/categories/大数据开发/Redis/环境搭建/index.html","985641405f896a254b8292fb72abcef1"],["/categories/大数据开发/Spark/index.html","d6b5d966e20746be626695d425ff674b"],["/categories/大数据开发/Spark/环境搭建/index.html","e021e55a409696c74a5c2a7dc6de0844"],["/categories/大数据开发/Zookeeper/index.html","dafda12cfc0526572fadefab7e79d9b9"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","647f2e449801aedf353c3101682a9847"],["/categories/大数据开发/index.html","85fc1f5569ac5fa690c65db1767dc4d5"],["/categories/学校课程/index.html","c6eae17ebb7dc4b6e861c722d1feabb7"],["/categories/学校课程/计算机操作系统/index.html","3627624157e3c1e0a3923e1cc1549f98"],["/categories/操作系统/Linux/index.html","d0952837ff838b7609f74128bf38fd1c"],["/categories/操作系统/Mac/index.html","19f542af0a2a7a48456803f2b82a4124"],["/categories/操作系统/Windows/index.html","56fc96c430b9635b262f39fa10361beb"],["/categories/操作系统/index.html","a61cdf14c6044db66e5a94b8fce7fa31"],["/categories/数学建模/index.html","33389d362805837f4589dd7bc0dca59d"],["/categories/数学建模/latex/index.html","785e6290fd02ae2c652abb1fc484e601"],["/categories/数学建模/优化类/index.html","93db241d6bd87096bf6fa19a94d60599"],["/categories/数学建模/优化类/现代优化算法/index.html","865005df05a00041ad2c719d9505726b"],["/categories/数学建模/优化类/规划类/index.html","0163a3cd8f1632345c00a0dc7ce62ba1"],["/categories/数学建模/绘图/index.html","9a3ced0050e198063a8981ae333ed7ee"],["/categories/数据库/MySQL/index.html","ddcb09e92f0ca5f68efed3e824b9417d"],["/categories/数据库/index.html","4922d2dfd2c687e40d4599a7232064d8"],["/categories/数据结构和算法/index.html","0fce7422ecfebc94c8b2d1664b662eef"],["/categories/数据结构和算法/page/2/index.html","98ff6cc58c3949f541754ac6c4f5d3ba"],["/categories/数据结构和算法/基本原理/bfs/index.html","b4423317b907c17a7ce4fe3664ce0b69"],["/categories/数据结构和算法/基本原理/dfs/index.html","cc6bbbf5a5c287f733a60a66cf05ddb5"],["/categories/数据结构和算法/基本原理/index.html","205aeeba5a17c0c133263516ab5847b0"],["/categories/数据结构和算法/基本原理/动态规划/index.html","3b5c51f3029228933491f76ce279b193"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","e7da52634f9d074284a21c445d0a54b6"],["/categories/数据结构和算法/基本原理/图论/index.html","0fb3b91bc5a67caeb4d4e10cfb3c9529"],["/categories/数据结构和算法/基本原理/字符串/index.html","3a45b53b7b947493f2096007734c4507"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ba517d1f8086b82041ca400dd90e87f1"],["/categories/数据结构和算法/基本原理/数论/index.html","e566de052ee5fa41af08f38f8fa59a46"],["/categories/数据结构和算法/基本原理/树论/index.html","e24630e6673546a3cce5387f9c813665"],["/categories/数据结构和算法/基本原理/链表/index.html","39bcb8e94c448723b49dc5c4fd68b37e"],["/categories/数据结构和算法/算法题/index.html","b80eb2e1bbfc4d32a52eb08f1798567e"],["/categories/数据结构和算法/算法题/二分查找/index.html","5a228e768397b7965ce0957f82541d36"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","558710c8e91d31047312ba2e2fd0f0aa"],["/categories/数据结构和算法/算法题/动态规划/index.html","51b9910c8a000735bdf007e23f3271a8"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","0e3d85669e6068467c81089e10b72ea5"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","180bc7e6f6e30bfc1729746bb4b1e87b"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","dc57a7ce5fb067c0b7cee4793b4f5239"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","7550d616d197b94ac67a777932f7d6a5"],["/categories/数据结构和算法/算法题/数论/index.html","aa9df1a04203e755fd2de6fd973ab977"],["/categories/数据结构和算法/算法题/栈和队列/index.html","24c8e2f70bf190d606271444bb4046fe"],["/categories/数据结构和算法/算法题/树论/index.html","8411bb6d60fd0c45fab55c3eb9a2689f"],["/categories/杂七杂八/index.html","a7506ba8e2c55a5146bd59c16ce6588b"],["/categories/杂七杂八/博客搭建/index.html","d4b8e452415ee76e0f8b0b140273caf5"],["/categories/编程工具下载/index.html","2e9ccdae1d27f3e47a6af8298a5f9cb0"],["/categories/编程环境/index.html","c9b6aa41b8905d1d4d1dffd8b864a7b0"],["/categories/编程环境/大数据/index.html","c1fd8d83d02b66092a8bcb8c65861c36"],["/categories/英语学习/index.html","8480157721b23cc40549cf165dfa3b73"],["/categories/英语学习/英语语法/index.html","ab53dbd61ffe6e9c6669e7315aaf03b4"],["/comments/index.html","fb646c011839a86feb533b4789e46fe4"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cc71d963a763920791a8e0e4306fc0ac"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","fad3661f4ea26f089327a6413a19510c"],["/movies/index.html","4c009b4dc36d6f595f4e23ed86430bb3"],["/music/index.html","e404909342ef57e118260dbad2c8b8be"],["/page/2/index.html","158ddf30db4764fb16d1ba76cdd57fba"],["/page/3/index.html","50c00b00d4f6e6347923835361fe01d7"],["/page/4/index.html","a48f8af6349bf122510661c3b553d8fd"],["/page/5/index.html","b1d977c28469d792a516e7b6a374421b"],["/page/6/index.html","2e1465af5586471e95721eea0a98c5af"],["/posts/1021360842.html","6cd4e1607a48850d2213192b572a04ec"],["/posts/1120620192.html","41cf22c78f1736c51dde04f682203368"],["/posts/1141628095.html","def56763cde622538e736c790f0150f1"],["/posts/1168613674.html","f50558c8e33f0638a25db7a4d0d05b90"],["/posts/1219920510.html","04bcc4d64264bdebfd3d7e221c49b2aa"],["/posts/1222166338.html","13dbca6b6572e527826d7d5c6557ed74"],["/posts/1259097482.html","0b3b55bd788f0647ce709fa6c29532d4"],["/posts/1271036369.html","ec6c631aa3a91b8fc1fd0b4e4abdf88c"],["/posts/1312847445.html","262b0ce029a4b4ba2c0c966b4c561f66"],["/posts/135355774.html","69d60cd23e016eee82992938a0abc38e"],["/posts/1375344716.html","c1e6d9c0b5e2cf6b2deed53344dc9008"],["/posts/1388991698.html","979bb1200b1520b168472e5557d0e80b"],["/posts/1410315814.html","18b1e7a3e9d4466963009939314573f2"],["/posts/1452790229.html","e9b8ac20f3b162debe1420df8723af93"],["/posts/1470079884.html","9c0819ac52133a84a8622350b90f414f"],["/posts/1470079885.html","e39af5c3d4de8a7085b14f7781c0926a"],["/posts/1470079886.html","894d9e5a56a45490f75be197bc7b0925"],["/posts/1470079887.html","97551d19257ef15589c12ef677d2f6f0"],["/posts/1498536549.html","b82179bf7258afa5bb2a3fefb56a1bc5"],["/posts/1539568593.html","23bdb8799e3025d6507fc85a38c80cf0"],["/posts/1547067935.html","b4e9a857af12229db3517a8185a12d14"],["/posts/1557866301.html","9cb9d4e486be6609e51e75d0598a859f"],["/posts/1571776361.html","6a12a8baf66033f3fe7af82616663a3f"],["/posts/1605124548.html","f8e1dd311bc89aea9c64e315daaec2ba"],["/posts/1633036852.html","a20cae720afec04a98b7ec4c3abab95c"],["/posts/1674202625.html","c26148f20ca26b43acca21afdde9a2e0"],["/posts/1765123828.html","33da9a0172d896d86d03db83722fe276"],["/posts/1767336200.html","97986a1f192cdc53b2545c4d734d8a75"],["/posts/1776114197.html","304dde9f84ce42a1ba330b619772b3f9"],["/posts/1817748743.html","68c6955341287ca3c4d0927cee61e81b"],["/posts/1925125395.html","f520b58855028549d70ce927f27ffc96"],["/posts/1966191251.html","0072ac5e4e1811ee3460a6fb817d8f85"],["/posts/1987617322.html","7b9107813046fb826fcd48a359bcb677"],["/posts/1999788039.html","35350153cb723529eb4230092d9a3020"],["/posts/2075104059.html","7aa3fca6baf7470e139b3e664058b6b6"],["/posts/2087796737.html","3e479d5a54885bb88ffba64aef899080"],["/posts/2106547339.html","df4527153a06673f261f7ed9e2871d2e"],["/posts/2207806286.html","36006f3759c7bd0ad73f28fabbc82038"],["/posts/2225903441.html","5f8352b2b2903a807e7acbefe55cbb72"],["/posts/2265610284.html","b0b3bc2f718967647268721d58f08f02"],["/posts/2281352001.html","0c1d92ff1557403465c619c3f23efdc3"],["/posts/2364755265.html","b342ddb0802752fd29e550e255e1b24b"],["/posts/2414116852.html","19ac73e731b55b4f391a1ba264b5d80b"],["/posts/2421785022.html","a9ecf65a26426923bf6bf5e1e4f8a610"],["/posts/2482902029.html","5c266a847b4ba59476279cf783a88b50"],["/posts/2495386210.html","a9104d55185df9e06107145db850823e"],["/posts/2516528882.html","2a2317763f8e0603406d41a0226f27cd"],["/posts/2526659543.html","dbb437f7ed4065caf0568b283583cafb"],["/posts/2529807823.html","1373f76ed8ed79dd10251f06158fd51b"],["/posts/2596601004.html","f29fa4eb2e1f40157b0f5bf08a62430b"],["/posts/2697614349.html","c330c89057c99eedfb41f39939cd6e51"],["/posts/2742438348.html","26c6f028c9b86c7f82ce404a22444909"],["/posts/2768249503.html","5a5c22ebaa5ce60e4090d1bb0825798d"],["/posts/2864584994.html","2765f13eadf3fe331d77834116b37a47"],["/posts/2888309600.html","919f73a32550734f3f7132bb75e64039"],["/posts/2891591958.html","4d89af251cde269b1704adee5921aaac"],["/posts/2909934084.html","6cd24281042b779a7c1182f449c10c76"],["/posts/2920256992.html","79ed663daddd901154aee5d0a2a6ec17"],["/posts/2959474469.html","5cfb76a9669faad428c39a8d1087095d"],["/posts/3005926051.html","60951c7cf56e4767966652a2787dbef9"],["/posts/309775400.html","2a24a48e21436a2b7bfa4ff1e2dcad6e"],["/posts/3156194925.html","5bacc523a1b30dbb70d64bb4d5c7b3a3"],["/posts/3169224211.html","1cf6a55b6d405190d320c4dede388e7b"],["/posts/3213899550.html","8765a060deb8f477233a0daa4c73f53e"],["/posts/3259212833.html","a785ad95905f0d675b89b99d5a376fe1"],["/posts/3266130344.html","83afa54493b30942d209147b7e3d1fa3"],["/posts/3292663995.html","11be809f831e78bb33f3b111eddf0a10"],["/posts/3297135020.html","2170851b00ae2d1ea72993e5c42fdafb"],["/posts/3306641566.html","996c38c1381fd0bfb7825c09e287a766"],["/posts/3312011324.html","0f3aaf368b8a913e740f4814f6bf5f48"],["/posts/336911618.html","eda9c5990f67dcf1847cf13c4a07fc75"],["/posts/3402121571.html","8543f96e451d695cbbdae7b0f5141216"],["/posts/3405577485.html","3744bde3aac728de44fe60b82b706081"],["/posts/3498516849.html","405b1d3546b0b49d0f0dff0b0cc3f0f4"],["/posts/3513711414.html","adc7e2133d5ef868a087830cf2d317c2"],["/posts/3523095624.html","610f7e2db6557045249821c0ffd6e80c"],["/posts/3546711884.html","39c6c252675729fdc389ad36fa34b46a"],["/posts/3731385230.html","60a9565eee36c904ff13b82385410d11"],["/posts/3772089482.html","9a887b04e8eca40cfeed6133cd0c3d14"],["/posts/386609427.html","6fdf67453c294667cfa51806398141cc"],["/posts/4044235327.html","f1917ad6dae14ac9929c841b80eec875"],["/posts/4115971639.html","6671fb95400ec8147f764f12950a75ab"],["/posts/4130790367.html","169e23057d87182989d96f297a6d1f8c"],["/posts/4131986683.html","17ab595d21691301102b89fe9007be90"],["/posts/4177218757.html","e742defcdb0da90c722314b7516b8674"],["/posts/4192183953.html","a4f9a0a2a0db830d8fa5235bdd304a21"],["/posts/4261103898.html","e7b10ef0b488c6ac51b7507fcf314560"],["/posts/469711973.html","0b8221b699cced11a88b305779932b41"],["/posts/482495853.html","643b4db764c0f0e53f5d88dc6cf25f34"],["/posts/488247922.html","23abb5daacc0c68351296fed8fa91851"],["/posts/517302816.html","433b85d22cf7c4b3401879e3ad5d2bb5"],["/posts/570165348.html","cf80ccad371fae3651d3dc8357bf6489"],["/posts/595890772.html","10563c17e0cadb000d454023942fc228"],["/posts/67485572.html","12992b71af2c446856130239213d190c"],["/posts/694347442.html","f48d6843130fc7671751672f9f626c6a"],["/posts/707384687.html","e55a4aaaad9474a284f5cdbc6267ef6c"],["/posts/71180092.html","ef2f30b83ec3ec36a8ac10402cffd236"],["/posts/716459272.html","d76b14b0f09cf1eb481ef04cccb37658"],["/posts/765481613.html","b7c37bb65f8ca23761e746c9c7d8a32a"],["/posts/778231993.html","1004ed04d66ff4de8625fa090e97afa3"],["/posts/795397410.html","c12a998faa6266e5f0f98c148d8c9fc1"],["/posts/820223701.html","b4d3b1a957a668a644b1f66053b34b67"],["/posts/830372185.html","f4522e3f970e742d924437457c0987c9"],["/posts/88294277.html","8663157428236c7d56d7a4bfe1854acd"],["/posts/939963535.html","e78b2a4e56c483c6093b60cdb6602bb0"],["/posts/983786067.html","5b6e7985d352840230f9fdab2d97318f"],["/sw-register.js","6dc8003e54add1b0b491a21d15130481"],["/tags/C/index.html","94ab971ce90ee82dfc78e696c2a60313"],["/tags/C/page/2/index.html","e4a439dae872de25f5db01a210050108"],["/tags/C/page/3/index.html","d84138b725548d4b9a79e91c80d69ca6"],["/tags/C/page/4/index.html","cbb8ca85fdf29f20dad27491febac245"],["/tags/ETL/index.html","51da83b8374fd8e89f0eb6ec46a4ae77"],["/tags/ElasticSearch/index.html","481838439d969ed53979ca661c812b69"],["/tags/GUI/index.html","92824262a389ce8d5cdb8d7c478c8809"],["/tags/HBase/index.html","b00f4a07fef32865fe79d3fe2d6debe0"],["/tags/Hadoop/index.html","dd1b4ff5416075ae22c9bf326c583163"],["/tags/Hadoop/page/2/index.html","35c09c453b7ec3df1d30d4f036d78190"],["/tags/Java/index.html","0507887df27e0114841913701424d832"],["/tags/Java后端/index.html","1165585a1b6f3aa6f841c96540819bd4"],["/tags/Java后端/page/2/index.html","16f177212a934ed9d5bc5f43e2c974b0"],["/tags/Java基础/index.html","e57bccf870838b6de2034b529a5a3976"],["/tags/Java基础/page/2/index.html","4a06e3cc08db988e7425c3558be20b83"],["/tags/Kettle/index.html","c73758aac053094dc471f048f3f268f8"],["/tags/Kibana/index.html","05baced6d86e105207791e9fbfa28b1c"],["/tags/Linux/index.html","de7dba9800602b8a3a39c17bd05670ed"],["/tags/Linux/page/2/index.html","829fd39b34409cadb14365b0569c7a50"],["/tags/Linux/page/3/index.html","b8dda4dc0d7a17dc596d2c446ffee799"],["/tags/Mac/index.html","ab48ab806a6707d89dda1b836612feb9"],["/tags/Mac/page/2/index.html","2b929f99dda546660ff498fec6b1e7b1"],["/tags/Maven/index.html","e6b4f175c55974f6c49354c1004a5cd6"],["/tags/MySQL/index.html","43dcaa25fb39db02b93d16b5eac43195"],["/tags/Python/index.html","bf0a904ef627d54c5e229382fafdc49d"],["/tags/Redis/index.html","52595b8a7408f89c5f3e7ac7537fbfe8"],["/tags/R语言/index.html","2b52b7c7faaa69f4efe03865157d8878"],["/tags/Spark/index.html","41c58ce6b44b23926e3e15dffa4c1375"],["/tags/Ubuntu/index.html","4d6495cddb926beef252432e99862eb2"],["/tags/Vue/index.html","b5cf02fbcdbc2abac599ca93f4f6a7af"],["/tags/Windows/index.html","20f841a1538db10f51d9531c6359c5f0"],["/tags/ZooKeeper/index.html","c3c7f6d20bd530b789eeaf2208188545"],["/tags/bfs/index.html","8dd9f7812a8c8f25093df147aab26f77"],["/tags/dfs/index.html","b71b3a4bf41cb60d4384e4a7fe2fa81b"],["/tags/folium/index.html","198ca07a11a59a4af0908e4c5078a5a9"],["/tags/git/index.html","25fd878b0ca0172f37c4d55915d0065a"],["/tags/index.html","b640d7d431a5d433d9b565a261a1402e"],["/tags/latex/index.html","27e690d78a30fe4f16313d981d778b0e"],["/tags/中间件/index.html","77caaf52985e34e5e73f204039b0762c"],["/tags/二分查找/index.html","c964368a7258468e05dad0f773cf4c2b"],["/tags/优化类/index.html","789c81093c5f46c79c0eecd0d2aa218f"],["/tags/前端/index.html","6cf2ec83b9074de1fb60dc65716285c4"],["/tags/前缀和与差分/index.html","243ced65801d003aefb5f2905f87ce4a"],["/tags/动态规划/index.html","217c400f4d9936758a4f93d67ff88005"],["/tags/动态规划/page/2/index.html","d1cfcf5f33a8860035dc287e62f54800"],["/tags/博客搭建/index.html","8f597e1e088f4c964e01ce266f91d21a"],["/tags/图论/index.html","b4f6ba2b0c06ee8b489c09568a977b1e"],["/tags/大数据/index.html","178fa2ec391a63404ed6431133b465a1"],["/tags/大数据/page/2/index.html","cea455aa8071f71679c0b23684576164"],["/tags/操作系统/index.html","a3b89bde838abbaf6064adbbc822eea7"],["/tags/数学建模/index.html","9b45b4518405f330a38e3e291dbd79f9"],["/tags/数据库/index.html","c1a6af6af671f4d32029069ecebeb0f9"],["/tags/数据结构和算法/index.html","146e14d274a3485955d85eb4182a4047"],["/tags/数据结构和算法/page/2/index.html","bde6b9ad5474eab4273f025fec21fc77"],["/tags/数据结构和算法/page/3/index.html","ef87893ce0686231d37ec171ef267d55"],["/tags/数据结构和算法/page/4/index.html","dcbefc1f949c5408aae82aea47a0467f"],["/tags/数组和字符串/index.html","c29a9a675fd933f4d638ac9b867e472b"],["/tags/数论/index.html","f339dbb44f2fe8760d30732f120fec61"],["/tags/枚举类/index.html","b14c82292f5b42f1066a3f25fb49a77d"],["/tags/栈和队列/index.html","421d431b3f1ec7f1cf198b939ecdeb0f"],["/tags/树论/index.html","8c014b3d3e8ca34ecda906df94dae657"],["/tags/测试/index.html","29a6f3d1ad2eb6180e4a2556d5eb602f"],["/tags/环境/index.html","2fb19b1ba1381070d986ba56318fc24a"],["/tags/环境变量/index.html","20490688044820bc64f2ab8503da2e8a"],["/tags/绘图/index.html","fb5277f81018a8fd029c8a8d6a41ec51"],["/tags/编程工具/index.html","ccb26a183717b429dad23ce0e79c2ce7"],["/tags/编程环境/index.html","851f2f9b67d96eaf1c2d29cbc207ee73"],["/tags/网络编程/index.html","00c7659a4c7a8f0ae8aa4bdaec5296bb"],["/tags/英语语法/index.html","8c9c7f62ab91e359c4fe2dd755ec4707"],["/tags/计算机操作系统/index.html","0dc892d94726efe7efb1e136b8f4eb57"],["/tags/论文/index.html","6cf5be90765ec1873d0392822aadd53e"],["/tags/资源下载/index.html","f53a72d58acc03602067af38e3b4b21d"],["/tags/链表/index.html","c8ca64156e2f16084b211b403ff5bb85"],["/tags/集合/index.html","601533ec03b95ed371b900c383c89a60"],["/tags/集群/index.html","a4d20694821fa0a1033e051aa42ce0d8"]];
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
