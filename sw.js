/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","20dfea7d43c78199eb6507d947acf337"],["/about/index.html","607d5d512a1837be714032c1917ce684"],["/archives/2023/01/index.html","b0945a5a093014d4bd43025c9cdf8dee"],["/archives/2023/02/index.html","5cb6b79b938a0cdb025828c475b5074f"],["/archives/2023/02/page/2/index.html","33fc1bfd7d398076497a495ed4a39333"],["/archives/2023/03/index.html","e34b33f9e9389aced82c77b16d7ce068"],["/archives/2023/05/index.html","e4060a41692488c6f117d323afefd3f4"],["/archives/2023/index.html","d829ced52b01eaf786f0e905170e43e9"],["/archives/2023/page/2/index.html","77c8efc04aa5274a7700b23634e7e895"],["/archives/2023/page/3/index.html","a1acd799eb28260196d333722efebc99"],["/archives/2023/page/4/index.html","4b4a10eb64c064bab7b11632d5032298"],["/archives/index.html","d45aadf31ebeb12066cc98d92eaebfc3"],["/archives/page/2/index.html","34fcfa22435622c150b5fcffb451a6dc"],["/archives/page/3/index.html","42d49133d173188852759e0d17c9de14"],["/archives/page/4/index.html","3e7acac03c842a4207db6aaefc8c36d8"],["/categories/Java/index.html","a76d32b0c6a9d17a1dce8a60791464bb"],["/categories/Java/后端/index.html","c95982cb4ce6d41c9290252a30ce7217"],["/categories/Java/基础/index.html","2f9ec2e388c12c3ef27fc39ec7b81b6f"],["/categories/Java/基础/集合/index.html","5792d4f7d3c8a7ab66825ef281b0f284"],["/categories/Python/index.html","fdcae39567ecf632f6b9d04e62f4eba5"],["/categories/Python/编程环境/index.html","f8d47d6e88332f7d5630e18868e92cfc"],["/categories/R语言/index.html","f1e3f54e25221a270575c93d6cc379ac"],["/categories/R语言/编程环境/index.html","ae8888f2c8123ffd737c359b9308d5bb"],["/categories/index.html","1bcf86f425e8d55dd1ae9b56a6291373"],["/categories/中间件/index.html","8134da2b20ae159843a38bf9594dd413"],["/categories/大数据开发/ElasticSearch/index.html","da90c524c34aa9db9205ead7336c5a22"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","d5b9fee8659866fdc7c1f6f4bbc3c23e"],["/categories/大数据开发/HBase/index.html","8e5ed6022d6f962e4d137a6f1a43c2e7"],["/categories/大数据开发/HBase/学习笔记/index.html","b7c37461097b8a506232ccffe39d3fe7"],["/categories/大数据开发/HBase/环境搭建/index.html","631ef8027c8ce9237396308d0e122326"],["/categories/大数据开发/Hadoop/index.html","7218175ca2d7c7ae2a7be28ae07f17a9"],["/categories/大数据开发/Hadoop/技术/index.html","e5761ee056f2c41495ecb99c3b3be189"],["/categories/大数据开发/Hadoop/环境搭建/index.html","bee567e6a1d29921cca94d3456c41793"],["/categories/大数据开发/Redis/index.html","17c9c79aadcbc2e9fdb921cee553e8d1"],["/categories/大数据开发/Redis/技术/index.html","acd14717321c0377fd740acbbdaa8ea3"],["/categories/大数据开发/Redis/环境搭建/index.html","f20d859ace3354a31d79c38d9dfde44e"],["/categories/大数据开发/Zookeeper/index.html","09f4637c3c5c813df19f090236051a87"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","65f33c508fcf83031e12fbcb5b8bf252"],["/categories/大数据开发/index.html","45535e1fed44b32ee6aa345d5cc9ce15"],["/categories/操作系统/Linux/index.html","c64fd8eee2472c73e5aaeb5f875e1c66"],["/categories/操作系统/Mac/index.html","1c2f2ba3cce4359a35285aa31527e446"],["/categories/操作系统/Windows/index.html","07e4233f45f957e7442f25ca346cd141"],["/categories/操作系统/index.html","55ebe118c8098cfa22210818c9cc192f"],["/categories/数学建模/index.html","12e15095580c27c4ff2cb08ee499cf69"],["/categories/数学建模/latex/index.html","0605167a9427c09267227b7831943716"],["/categories/数学建模/优化类/index.html","9a73f1e359888b7b4f0b693c611270d6"],["/categories/数学建模/优化类/现代优化算法/index.html","18ecdb5fe6c9a97bd599d438b7710a8e"],["/categories/数学建模/优化类/规划类/index.html","d5d0577ead4262a74885792764cad7d0"],["/categories/数学建模/绘图/index.html","043c436044dca96ecf4704b8e3c99805"],["/categories/数据库/MySQL/index.html","48e4328283732b092c0eae1b0f3a7a5b"],["/categories/数据库/index.html","62a2c83abcdf3cdeaf33b92114da1ea4"],["/categories/数据结构和算法/index.html","4a0136a86ee5ed7f88aa5870054995a2"],["/categories/数据结构和算法/page/2/index.html","f0d4013872137e3b1ce0916e1e97e868"],["/categories/数据结构和算法/基本原理/bfs/index.html","7015d047be097f99a27ff2198622cb88"],["/categories/数据结构和算法/基本原理/dfs/index.html","2bae81cd432e7a4bbb90d021f4c30d2c"],["/categories/数据结构和算法/基本原理/index.html","0c5598b3490769e323d95a767ceba362"],["/categories/数据结构和算法/基本原理/动态规划/index.html","ae08d0aa089af5bde96075c7b01018d4"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ce3b67cedcc8046ab6acf007dc746632"],["/categories/数据结构和算法/基本原理/图论/index.html","250c53f0dc3a7c117f7d0f4a5ff05171"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","ca530506490416922ede089264c3fabf"],["/categories/数据结构和算法/基本原理/数论/index.html","b277783824a299c36559953848b4dc4f"],["/categories/数据结构和算法/基本原理/树论/index.html","f419b096d843eee12bddc7fd7d308eb6"],["/categories/数据结构和算法/基本原理/链表/index.html","1f988db130ea6c5d4308173033c61605"],["/categories/数据结构和算法/算法题/index.html","ae13335f10eab5c366e59e5fbcdccee2"],["/categories/数据结构和算法/算法题/二分查找/index.html","43fc19552986f388c2db0f834f8bd1f2"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","6e5e1960e263032e54f316bd4c7e33e3"],["/categories/数据结构和算法/算法题/动态规划/index.html","b5d21140aeb80e6a6f0e6584d2fc087e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","fdd903ad2a47d6d297dfaaf4021feed7"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","945a2f8c094a417a37828279950d3bc3"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","67b53e5210eac14c54509c8ed8457be1"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","388adbf1ea52a91f3d9a9cd31d0f33d7"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8a4e30aa5a91e8a6f9c4787b777c896f"],["/categories/数据结构和算法/算法题/树论/index.html","56416429a3362c96d8f2736058a32665"],["/categories/杂七杂八/index.html","1aa60c6e350c5301f952322e6d825664"],["/categories/杂七杂八/博客搭建/index.html","9d8142da863156f848cac363ff86cfc0"],["/categories/编程环境/index.html","87e63a8ae2f5ef926dbeb578a3244295"],["/categories/英语学习/index.html","0b3c1665183bb69409d7a5fe65792c41"],["/categories/英语学习/英语语法/index.html","13eeb8139a5b8c3659b813e49e0a3ba4"],["/comments/index.html","44d6bfa5e13342522868e11c99137936"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","2c816f9bb6e9ded081937d5939d423e4"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","c8556e15cc3dae5d19aded21a43e2e77"],["/movies/index.html","7adf376f0a64ecc8ac11e72140637ff2"],["/music/index.html","7bea247a785d8751ef776cff4c4b97c4"],["/page/2/index.html","07e34ecc31395e44c6b25c8d1abc9199"],["/page/3/index.html","2230af457a6698bcfabcf5486de0c0e8"],["/page/4/index.html","6cc05bcad6fe46c7dfdbd0cfe3775475"],["/page/5/index.html","60437c191a253959d923f132d3ae27ff"],["/page/6/index.html","4e0563bef33c4361fe5a7e9ff34d1140"],["/posts/1021360842.html","e52cfdb547631dc9ce70fbc54717d1dc"],["/posts/1120620192.html","e0ba459b9db75193a29bf7078a69a582"],["/posts/1141628095.html","c57a24799085ea98c828c4b980402b8e"],["/posts/1168613674.html","a176754ba85e5f967c48f237818840db"],["/posts/1219920510.html","cb2b893cd2e3d9eb3685ddf476890f76"],["/posts/1222166338.html","3c6a4332a0f7e9c051569732858b7a77"],["/posts/1259097482.html","75fd8568452c0b80c70736d8676fcd08"],["/posts/1271036369.html","135f9e251295825ab856bb60ded2c403"],["/posts/1312847445.html","d45b02fb22f8f0cd880606b5e9042002"],["/posts/135355774.html","0a3385d8de3977bf58ec4af1c47c92ed"],["/posts/1375344716.html","699248e437065d74d4476e0e0b95336f"],["/posts/1388991698.html","c1a4f9a851bbe6ce28af689863723b59"],["/posts/1410315814.html","d0fdc6856d5f0b8f78d7b81f55d08574"],["/posts/1452790229.html","094cd282e3b5dd99dd4b18be20187164"],["/posts/1470079884.html","d6d563804da664342920846a549655b7"],["/posts/1470079885.html","acb0a4375ab3bf60b3330e969ea711d0"],["/posts/1470079886.html","ccd0b525ce103f6a43a86118dc62a729"],["/posts/1470079887.html","2c315aa89d01346456bc66859b93f769"],["/posts/1498536549.html","f6e093fb9cd2949d65abfeb3a2f25458"],["/posts/1547067935.html","3ef1a26009e186025981a35755a1b9b1"],["/posts/1557866301.html","37bb60ba6b2ff8c83044f515de5bd044"],["/posts/1571776361.html","ec79c7c0378c713678407e9a426cca5d"],["/posts/1605124548.html","343e558fb21603a3833359ad59fba1f1"],["/posts/1633036852.html","6f93d63b86eba0aa54323a6123cb4e05"],["/posts/1765123828.html","1867ba04cb49c96eb372c08608180723"],["/posts/1767336200.html","98e281d7bf1d34f5f92bb97cbde3932d"],["/posts/1776114197.html","b3b524c1971e573b8913dfe69b12a0a1"],["/posts/1817748743.html","9d679260f65287b257f577ee220a91c7"],["/posts/1925125395.html","0517c606772c3b286e638d5e7848a1ce"],["/posts/1966191251.html","b62a6681596fb756dd33686d87418c36"],["/posts/1987617322.html","d334ee07f8475d80b828a3a7bb13c450"],["/posts/1999788039.html","7b6d0076e5951793a18a78545698186b"],["/posts/2075104059.html","67b6f5898ba62446e249147ffedb9332"],["/posts/2087796737.html","008d1953934eafe18cc7a52a19198f38"],["/posts/2106547339.html","61417194b1f3fc7f3d76245c2fbe7f7f"],["/posts/2207806286.html","56edba8f15c808ed4e9ddbbca34a7e5c"],["/posts/2225903441.html","6e7c138e4178f97a7f4e9dfcace2d2d9"],["/posts/2265610284.html","6e37dd13d64f84b7200fb3cd91bf2a6b"],["/posts/2281352001.html","a8bad4e8f0a82cdf64d08d49d113cd30"],["/posts/2364755265.html","aed722469c31a7f7c2f362aac170e73d"],["/posts/2414116852.html","8de98d1bd5d785aef4ed63ee54738bcb"],["/posts/2482902029.html","b777d4c04b9b62f12f0d17809def3f47"],["/posts/2495386210.html","f5188c1a6d0ae3e3cf05517b8e51151f"],["/posts/2516528882.html","6b91600fd53fe23ce2a8d3b4cbf9a9f5"],["/posts/2526659543.html","d80a5de6a14c936d6eb7b480405aecc0"],["/posts/2529807823.html","a4b0b5b10ed150d989633ebf5d8fccdd"],["/posts/2742438348.html","5161da9e4ba3cd2528886858392306f0"],["/posts/2888309600.html","80d9b9ba821a73e86609b65167b8dfcf"],["/posts/2891591958.html","931c6eeffa84074790c4a867e9e35cb2"],["/posts/2909934084.html","7dc3e43eaf0faca06038e6de819e6caa"],["/posts/2920256992.html","fca8f5ed2aec7f67bc40cdcff8d9f0d3"],["/posts/3005926051.html","6ee46e2e60ac10375ba13a1373e18449"],["/posts/309775400.html","80907fec706f859f0b7d18d21ba0dd75"],["/posts/3156194925.html","31fdf65f0a64b434f718f950fa5e140f"],["/posts/3169224211.html","df8509f0aa1a74fc6d82aabafaf6e479"],["/posts/3213899550.html","ce53e5c42cd055fd67b59d687b98473c"],["/posts/3259212833.html","7d04c5a0523b40ec48556c63e71c2375"],["/posts/3266130344.html","5517bab805db1b523afa51c6e0adf5f1"],["/posts/3306641566.html","6a96674ef68b21153e5f288e8c6404c8"],["/posts/3312011324.html","02743deb6d6efdd79b5e79491a08e288"],["/posts/336911618.html","764a784c7ecda88a8d3e7de243defa8c"],["/posts/3402121571.html","eded7ad13023c093984d62858ae9b859"],["/posts/3405577485.html","79929dab343dbab15c7bd4449e813312"],["/posts/3498516849.html","06f77d067853318f16bf5b471a0c6fba"],["/posts/3513711414.html","8c29281d0769c240b6a52bb99f200bf8"],["/posts/3546711884.html","60e253cb824b3c797ef0e545c26abb4b"],["/posts/3731385230.html","5d516f419aea664623917c3536fa5f95"],["/posts/3772089482.html","c204e463f679fc4a6bbbdfb8c6f73adf"],["/posts/386609427.html","9f7a0904dcb771bb54cdea5144f8d32a"],["/posts/4044235327.html","0aca5cd7a43e977814976003fcab850c"],["/posts/4115971639.html","b0b6f7155806b6ed33dae51d6fe5e446"],["/posts/4130790367.html","7d1e0a086ee03dd75190b3b935711a7e"],["/posts/4131986683.html","ba99dc36cde3a4af7005033eee3cac9a"],["/posts/4177218757.html","5ab388efa079b5e2a6db6a2103a2563c"],["/posts/4192183953.html","7b9b89e5fc3bd842428b912d0855e6af"],["/posts/4261103898.html","863f02c5ef71d34e5cded55dfc4e483c"],["/posts/469711973.html","0d4e38e742bc8f41c42c82f14414f383"],["/posts/482495853.html","8fcd170c5ffe81bc133f1327971bbc35"],["/posts/488247922.html","200816312bcf43de806b709b65df96ff"],["/posts/570165348.html","91e6fe7969b2812d4b1f4fb14be85ad4"],["/posts/595890772.html","efed023ea13066e1e7512bfe9503a0c6"],["/posts/694347442.html","87fc6341152767c1d9c6d565ec000071"],["/posts/707384687.html","ac2fcc1b2b0e807a87ed62f6b6a28f1d"],["/posts/71180092.html","a22feeed9c76e8db698596ebf0b2ee6b"],["/posts/716459272.html","cb03dd037bf95a810a899c287db1c502"],["/posts/778231993.html","b8b8fb45a7c6327e29fa791da75a0a78"],["/posts/795397410.html","dc7898be064715c0fd654450cc4a21d8"],["/posts/820223701.html","b818e7bfe2dbbc83f869a6aaa423db5e"],["/posts/830372185.html","a103d167c15000f0215ad11e894ef851"],["/posts/88294277.html","c3da2fd8ed4733358664ff65ef35ae0d"],["/posts/939963535.html","627297e0fbf9fc9ee8763d204e1351db"],["/posts/983786067.html","fab1fe938c03f80f969f46f23359a9dd"],["/sw-register.js","432030ecf53f12e68b6b706f040b11e8"],["/tags/C/index.html","c8ede3cd1da318bb3d34877137da86f6"],["/tags/C/page/2/index.html","378a9b1eb7ce9fcdefb019072fb38211"],["/tags/C/page/3/index.html","7a0f3d41d67194d2fcc3898fa7692fb5"],["/tags/ElasticSearch/index.html","c14463ad8fed6423c9d97419c48a0937"],["/tags/GUI/index.html","1cb5696365e4ffc10126a2f682187b03"],["/tags/HBase/index.html","4fad07e927d23693685762b1ac8864e0"],["/tags/Hadoop/index.html","eba8118b05997b2ccdbb5d48ed25b72c"],["/tags/Hadoop/page/2/index.html","4d219b2057d54a4af37d204078805a81"],["/tags/Java/index.html","7ce233e95af0487cd8985f316e667eaf"],["/tags/Java后端/index.html","b6847af92efefa86b850cd1b010f1aff"],["/tags/Java后端/page/2/index.html","c0852666b0c08dd71a54ebbee4f70b98"],["/tags/Java基础/index.html","451b3bb764932c1d25fae66c1f1eb205"],["/tags/Java基础/page/2/index.html","7ea340fd39d72cbb5acf538969b69f39"],["/tags/Kibana/index.html","699c12dba9a0ab402089dc61255e49e3"],["/tags/Linux/index.html","15aa3932715a81e9d0a3119ccff1cc71"],["/tags/Linux/page/2/index.html","a432f19870ce6d80f05521ac7f1d11ae"],["/tags/Linux/page/3/index.html","3e51b277c604ee1f1ab828fa4f2d3c46"],["/tags/Mac/index.html","ff4dc4a05a1f86f4dc7613d90547bb2b"],["/tags/Mac/page/2/index.html","3e383045e8ff56cacb8ba0c65c0365a7"],["/tags/Maven/index.html","27f861334958698318bc7dcb26edd6af"],["/tags/MySQL/index.html","c73295f9af75b86bc53c74b5f2e8ad29"],["/tags/Python/index.html","057c7679f782d9e8d63a62ae67aae60b"],["/tags/Redis/index.html","c970f287b917d5507045fb220c2ecebd"],["/tags/R语言/index.html","146acf83331b09649f50fa057ec2eb41"],["/tags/Ubuntu/index.html","5ea6fc87499a2832e7b0a1659576f157"],["/tags/Windows/index.html","cc1016417a1a0fa43b69d283c59b218e"],["/tags/ZooKeeper/index.html","e36820fa6ac25085a6623bcd21212d59"],["/tags/bfs/index.html","7ac728e65c1f4865ff8557a96f820d22"],["/tags/dfs/index.html","bc64e4c7831b719e74ff841bca3dc06b"],["/tags/folium/index.html","1a7b4de10ca24252e607c4031aeba8db"],["/tags/git/index.html","c222afa22184091d6b102969bcab26ea"],["/tags/index.html","71543b172d2955f669a37c392fcbbed4"],["/tags/latex/index.html","7e55a0b6a9c4a48b0213e2d391ab1554"],["/tags/中间件/index.html","949084fa4f8375d1bfeb0b1a62e023b6"],["/tags/二分查找/index.html","a259ab306fa4b6173e284bc09d4fcf96"],["/tags/优化类/index.html","35f952c54d460030e846f2cec688a2c3"],["/tags/前缀和与差分/index.html","dd089bde01a2d644bd275ec01efde23f"],["/tags/动态规划/index.html","a892a63a3c9742a46837ee089e776cc0"],["/tags/动态规划/page/2/index.html","751d5c5caf4a930b90403e1fd67b4a5d"],["/tags/博客搭建/index.html","3bffa54dba5e34101aea1900e5dff2ae"],["/tags/图论/index.html","5a71ce0099d8acbdd54e9a034c93ba21"],["/tags/大数据/index.html","f97043df804567bcf49bb67a24425fce"],["/tags/大数据/page/2/index.html","2bdf6aebcf3d1840315f36bb940928df"],["/tags/操作系统/index.html","9c67f3479e46f71dd2345034f9875d6b"],["/tags/数学建模/index.html","e3a42121d093288a8c74f60040d258ae"],["/tags/数据库/index.html","35bdb0b2220943d1cdba7185e29ffba7"],["/tags/数据结构和算法/index.html","7e008a9e2e573e866e9e0b5286fa3c20"],["/tags/数据结构和算法/page/2/index.html","78a90633f59656b66b4f0db5850861e3"],["/tags/数据结构和算法/page/3/index.html","536e85c2982e71694afeb08eec2d170f"],["/tags/数组和字符串/index.html","6368b696fd3bd251332726763cdddba2"],["/tags/枚举类/index.html","ba15dc9eafee523df01cf32af5eeae43"],["/tags/栈和队列/index.html","c6c20452c8b50ce56195e25c50612756"],["/tags/树论/index.html","87974b13a84c5a1059e7cea09d36a963"],["/tags/测试/index.html","a728ad078be10816983a0086c832404c"],["/tags/环境/index.html","d3322cee0716bee5a954a45c92a31410"],["/tags/环境变量/index.html","7bb1ce89a1ed1f0b1788825b922c6f05"],["/tags/绘图/index.html","eeec8017921f2415d48f519fefa451f4"],["/tags/编程环境/index.html","0363d514c54e7b45abc9119e176866f4"],["/tags/网络编程/index.html","0c1479562c2eea563a535415a3dd4fd1"],["/tags/英语语法/index.html","c27802a1d1da90f92327b26a998e7b2d"],["/tags/论文/index.html","d41884c7982db9faf965a74191d40bc7"],["/tags/资源下载/index.html","bfe64aabbe1a67921cd7b11b352b8bde"],["/tags/链表/index.html","ceb75861bcfc16f0099b22e344c45be3"],["/tags/集合/index.html","0d3a5691d9ea3f55dfa0082c8c4732e0"],["/tags/集群/index.html","79db5fb1414f257f9bb3c2f74459fa2d"]];
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
