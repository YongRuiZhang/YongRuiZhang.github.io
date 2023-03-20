/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4a109bca2360dc55fdbbed92adfb86d6"],["/about/index.html","326f3c6f71a51bd1953da030a5c4fa5c"],["/archives/2023/01/index.html","78774c3b4f973e6790668cb92c3a5aa5"],["/archives/2023/02/index.html","2e62a56a695800c264c1f86c9d8aa33c"],["/archives/2023/02/page/2/index.html","be73f713ba37dbfc1aefd700df4c1dd7"],["/archives/2023/03/index.html","8718bfe81fc4d77047dd18122c7a2895"],["/archives/2023/index.html","22fdd73fb6b58d2120edd1c9a66e4f56"],["/archives/2023/page/2/index.html","b711a211e5fdecc671c30ac314121675"],["/archives/2023/page/3/index.html","e1632cd3598fc1b68c59506b8dca8181"],["/archives/2023/page/4/index.html","15de0bbcab6b4a2e649863c503249d91"],["/archives/index.html","26c9f460fa104e124cd7de78a3fe5716"],["/archives/page/2/index.html","4055f747ceaec02ca856bcb88e89a6df"],["/archives/page/3/index.html","b050e38be451cec2e9e291fbaaf3a12f"],["/archives/page/4/index.html","59da7ae5feb9ce886cb7b77ba3ceb2c7"],["/categories/Java/index.html","5868939d56294834725436ce6b938e15"],["/categories/Java/后端/index.html","0a83cad176e2c0ac107fc138ea5a0981"],["/categories/Java/基础/index.html","4bce356684e1cbb593964dc8e0dfe467"],["/categories/Java/基础/集合/index.html","92dab923aaac3e48c8559be96ea1b70c"],["/categories/Python/index.html","3473b9c1eb411e030af8a9dd1b9e5775"],["/categories/Python/编程环境/index.html","02bb6d3a3e16265708cb76c3cb9851d2"],["/categories/R语言/index.html","1143b22511c552bc20e9a89bdd2a9157"],["/categories/R语言/编程环境/index.html","82c82bc5f7b0c4f0a3eaf3f03cd727e7"],["/categories/index.html","2ff2c0a7198cf183677f22c7a2fd92a3"],["/categories/大数据开发/ElasticSearch/index.html","72dd134b7dda23f5bda0ec0038ef7cac"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2c1f58f50ecd35cc8ba6fb30687594e2"],["/categories/大数据开发/HBase/index.html","7322afe57dfbb846f96b1b00dea97543"],["/categories/大数据开发/HBase/环境搭建/index.html","dc1241b1bc6bc14ef7a83a88526c3126"],["/categories/大数据开发/Hadoop/index.html","af7974f2f824a737bfaf91086e823d6d"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9bfe173f09a9c11f2f9ac7da11c8d044"],["/categories/大数据开发/Zookeeper/index.html","b254020da14692896b7dc157f378885d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","794047f917b543d14add150e0cb75561"],["/categories/大数据开发/index.html","92c7584711336c8b9a5115f23f639ee2"],["/categories/操作系统/Linux/index.html","09e28d8c18c676aa8c9d792371c2819f"],["/categories/操作系统/Mac/index.html","7e504c30187a4545eb45f56daf57e4e1"],["/categories/操作系统/Windows/index.html","cc532fd440ab59cabba895df3f89147c"],["/categories/操作系统/index.html","ed6333af90d7528777f17038f2d6f087"],["/categories/数学建模/index.html","6e12527a02350c2ca2ba6333cbfec3e0"],["/categories/数学建模/latex/index.html","ac364383a6c2a196ebda2119a3836405"],["/categories/数学建模/优化类/index.html","aaea30ba9906ae7aea17d95e0897cc6f"],["/categories/数学建模/优化类/现代优化算法/index.html","4967e4542e91a397cdcb2cfba37feae9"],["/categories/数学建模/优化类/规划类/index.html","b1edb1df9b951665ac7f915da4cb67bb"],["/categories/数学建模/绘图/index.html","1d01172a39755b45ed40289d7ae13928"],["/categories/数据库/MySQL/index.html","deac8f24e249e2df28143e5897f089fd"],["/categories/数据库/index.html","118572376e131f905538d11067e43892"],["/categories/数据结构和算法/index.html","cd7d47ce3eb911944578641cdbbdfb06"],["/categories/数据结构和算法/page/2/index.html","4f3509b0153f698e998b91a1cfd6dd6b"],["/categories/数据结构和算法/基本原理/bfs/index.html","6d5fe91920e91c1e7883a64dc9c47ec6"],["/categories/数据结构和算法/基本原理/dfs/index.html","67d852856d3017576fd89069eefc3265"],["/categories/数据结构和算法/基本原理/index.html","4906232e5bceb399488089f6e171d4ed"],["/categories/数据结构和算法/基本原理/动态规划/index.html","33ffa5a27c33ac8e9ea7709bf36cfff3"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","cb128e1e52d7138f94936a1519f6a241"],["/categories/数据结构和算法/基本原理/图论/index.html","153c3f40905b3a4d34ee58b499b3002a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","9b08afa4aa33d6a0a97106a73674618b"],["/categories/数据结构和算法/基本原理/数论/index.html","fd12d03b5600ddc2f68aa2b2f3c7bbee"],["/categories/数据结构和算法/基本原理/树论/index.html","5704a0821be8766a408e77fe7fb6b689"],["/categories/数据结构和算法/基本原理/链表/index.html","08aa335f44392b6dbf6b911c43271314"],["/categories/数据结构和算法/算法题/index.html","b46b071cede791285a674a9d94c09054"],["/categories/数据结构和算法/算法题/二分查找/index.html","90df80ff2ae4ee01e700a1347ebac8a0"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","8738e876f0c6bc31b674f3840f914049"],["/categories/数据结构和算法/算法题/动态规划/index.html","274384428c9637be4f63fe5372b2a0f0"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","9d777b2c30dba476c8c51071923866ab"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","2492da80dc6c2cc32e7302c432833e54"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","7329e52ae07e867755da51d78109601d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","7c626e7b36c14f889d3badcd2c303a1a"],["/categories/数据结构和算法/算法题/栈和队列/index.html","649bf0d23088c449ae68cfe9b9a7912d"],["/categories/数据结构和算法/算法题/树论/index.html","1217f0bf218ad579275a4bd526df7fdf"],["/categories/杂七杂八/index.html","02ba9c363f6ba5f0cc3eff362e9809d9"],["/categories/杂七杂八/博客搭建/index.html","93114518b9e73b4763c933049026fd4f"],["/categories/编程环境/index.html","cb71bf603c5e7497d107101a90d0cc46"],["/categories/英语学习/index.html","b2ba6b56f87971edef7634524f5fc57b"],["/categories/英语学习/英语语法/index.html","2a1e538059f22fafff904b4787e6e26a"],["/comments/index.html","847853526a6cca632372051b1e785a44"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","152b1df7bc760e0cec032d7eec50d72e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","863ad9ac34d7ccf8b35c2d80d4a997f6"],["/movies/index.html","7b61c93842d9ee9d57a8871fa574a96a"],["/music/index.html","fdcee2691eef1fb6f1556b969532c856"],["/page/2/index.html","d7bac9111198a9594c94b4a4f37cee18"],["/page/3/index.html","b355ddbeb5f95af19723ff5dcbf731c5"],["/page/4/index.html","8e35e0bc883f86cc90d451d464e83cff"],["/page/5/index.html","9d015ed5b197ed45c9529129b64a7bf8"],["/posts/1021360842.html","169b9c180d20bb212c47c4c6b058e5dd"],["/posts/1120620192.html","46e6e1080c878b2e4c3aac0d64cd40cf"],["/posts/1141628095.html","cfcd53345917373976ae33d3d8cac246"],["/posts/1168613674.html","4b65ba7c8b490af76f8d6e6b7b6d79cc"],["/posts/1219920510.html","b56cf793fc2ae054a8858ae558918179"],["/posts/1222166338.html","a35588a50560df55107f2713493265ae"],["/posts/1259097482.html","4819e43afc4893a9aa6dc89093df3ad6"],["/posts/1271036369.html","12f094761c40ab3e125ec1be6bc58b73"],["/posts/1312847445.html","bbb1224ae3e072ef4c8b0cfd2825f4b8"],["/posts/135355774.html","5a2888cb7a2acf4ee7fc5f2659a39b5a"],["/posts/1375344716.html","f5c55f4f5b80d468b798b5fca5396ef3"],["/posts/1388991698.html","90a305ac2bff127d1cbb767fdb372b6c"],["/posts/1410315814.html","1a38a53dfb7b3181e0021add431e0b70"],["/posts/1452790229.html","6cd76b2655f382ac33ab1ba75a9f78a2"],["/posts/1470079884.html","e1fd04040c1af0b55b32cd0cb8662c10"],["/posts/1470079885.html","53d8445fd767371edad8eda442838020"],["/posts/1470079886.html","8724cb145a207e486ad56ecbee2acd0f"],["/posts/1470079887.html","abc9d165362d552be3c4bb8a4ec66b5d"],["/posts/1498536549.html","ae5a5ff8775a59acc82f8e4f7f9b72ae"],["/posts/1557866301.html","caaf2922bf00b4974a208256db3c3943"],["/posts/1571776361.html","9ecae4ede1dc4137295dac39680788d6"],["/posts/1605124548.html","53e465a599e2fa49b1eb79dd76387d35"],["/posts/1633036852.html","febe262e0c30892aaf0cb99e855fef6c"],["/posts/1765123828.html","f068156fb27acfb03a24cb067b2370af"],["/posts/1776114197.html","e25db25f69b84f0fdcfd215ef785275a"],["/posts/1817748743.html","3a67c52a721f76e5c5f66b7a687aeec4"],["/posts/1925125395.html","06de55fd4089b75a65d4be5742c8cd91"],["/posts/1966191251.html","600fde79af4a38e777a71d259f5c7db7"],["/posts/1987617322.html","eec88087495df138d4616122ce93ff2f"],["/posts/1999788039.html","ffe5bb7bafc742144cb325afd4e0086b"],["/posts/2075104059.html","ca15fa0fb287a95da64422af97a934c8"],["/posts/2087796737.html","c07c3177c4681d50019109be73ff5bf8"],["/posts/2207806286.html","d128d1fb2070837ef448e9270c4d5ed8"],["/posts/2225903441.html","bb2a85e28f4458ed072b15e6e2244eec"],["/posts/2265610284.html","db03d739ec5cd995eccbd01b897315ee"],["/posts/2281352001.html","3f9e0269dddbd6e2601186ea1999691e"],["/posts/2364755265.html","7d3ef480c76397a6ef58a795a64bfbb5"],["/posts/2414116852.html","84e94399860df5c7b3901dc806c4abe4"],["/posts/2482902029.html","43091f6bb6861545fca83826aa5ea1bf"],["/posts/2495386210.html","9706174f46588e27e155124239717a1b"],["/posts/2516528882.html","3cc3359e4b2f88477ba9e560b0341c42"],["/posts/2526659543.html","80020a797d2cbad2980130c872194940"],["/posts/2529807823.html","548fc62513c1d98850ae027c1f92e149"],["/posts/2742438348.html","8b6786b7ad177a857c0415c6f132fc55"],["/posts/2888309600.html","6675bd0e6912fd69ed91241835ee066c"],["/posts/2891591958.html","f4ec2eb7afe8ec1a34398723d4ab81be"],["/posts/2909934084.html","81c2b16d91ec374bc104414f937cda02"],["/posts/3005926051.html","9ffed82d156f871ea58e6bdce03a1115"],["/posts/3169224211.html","79d5622e26fd93c5d9d6f7428a9f7859"],["/posts/3259212833.html","eb3759638aeab5465b9e12998a5e2e93"],["/posts/3266130344.html","02e5cf2dc46f5351bb439491ebb1280f"],["/posts/3306641566.html","43fc3ca730011c4c235ed1817b24a344"],["/posts/3312011324.html","0ac59f4e2e3679b9d3f9e6ed9f04b5fc"],["/posts/336911618.html","b444ed04d962a45a4e9b8abb69826d5d"],["/posts/3402121571.html","b1b276733842a423d1c9e03261858d05"],["/posts/3405577485.html","a79c53d8366b98df590ab4e5641ab559"],["/posts/3498516849.html","06c3996706c16dd23bcfdaadf833f233"],["/posts/3513711414.html","8abf57e3c1c9124471b1805704d1e1ff"],["/posts/3546711884.html","4572644e9d873ed2b75c8f236d5f0b93"],["/posts/3731385230.html","bf70dd7b7b035199fcf6773de88fe7db"],["/posts/3772089482.html","478012877d8e2d8c607ba159cd54d965"],["/posts/386609427.html","12ffeeb970bfac6e3358de2cdff623f3"],["/posts/4044235327.html","8535531214379144a0aea3728f8afca5"],["/posts/4115971639.html","888a6207ce43f36b3de1807e03d245b9"],["/posts/4130790367.html","547fe1ccaf087042b6f27aeff096661e"],["/posts/4131986683.html","c831d519302fb59efcd4e0d1f796dfd1"],["/posts/4177218757.html","30ddf4eaff32200478bfdf427e9f177f"],["/posts/4192183953.html","e40b62474d7195f0a9ba41ba4436bfec"],["/posts/4261103898.html","3b383e53b01698ea2451d6925cd1573a"],["/posts/482495853.html","7da18f3515753ac3793533fb81708d38"],["/posts/488247922.html","bc2f53d1898bcf4a99264df071349157"],["/posts/570165348.html","50dcc198845b2591c033942f76ac0d24"],["/posts/595890772.html","b42dcf7b56b24ab9bad4dbc1f417d7e0"],["/posts/694347442.html","3e503b778441ba8e10dc256eaef7b52e"],["/posts/707384687.html","1072901e0c85989ad8018f693b8be5e2"],["/posts/71180092.html","d4200d899602d683e470ccad741c041f"],["/posts/716459272.html","afa4b58adfe4a666cb088146727bb78c"],["/posts/795397410.html","b9649053ab227827ffbf533f3206444c"],["/posts/820223701.html","130c5671599ad02eb0c727aceff1d462"],["/posts/830372185.html","d76fadcaefa0fa4985ae31d38d88fc26"],["/posts/88294277.html","0b18fd5e96f1f0b019a6ee1599420cda"],["/posts/939963535.html","a7f3f4867eff63fc5ad8c34aac0bc096"],["/posts/983786067.html","dc3ba07ba1e5d08eeb5cef3fdf24a551"],["/sw-register.js","1d519cc7b841f9b65e28f09e5e494aa3"],["/tags/C/index.html","2b30ca909163a0db4caa9d28d24fd877"],["/tags/C/page/2/index.html","c43d595fecc99e3d652efdadf1757488"],["/tags/C/page/3/index.html","cf4574784eb8b68a33640af3a5ad3e15"],["/tags/ElasticSearch/index.html","59bd172fedd4dff5db6aea83470dc8a4"],["/tags/GUI/index.html","3acf28b303a31a22dceaaaa5347b866a"],["/tags/HBase/index.html","a70276fa35fa6bc68111c0393cfc3261"],["/tags/Hadoop/index.html","67533011c409670bad5528aa8f859607"],["/tags/Java/index.html","c3143a623f6506462b82361129c2cb72"],["/tags/Java后端/index.html","0e7cc9be48f1f1e008b8e1228f995f43"],["/tags/Java基础/index.html","cf4915d3240db21f3a782053dfc03dc9"],["/tags/Java基础/page/2/index.html","eef24d782cef7e99606c742dc0d986fa"],["/tags/Kibana/index.html","977db7a920495d3f67a1e1a35f5d9f9c"],["/tags/Linux/index.html","5b083e1febef49f7efa67755cd604625"],["/tags/Linux/page/2/index.html","4e2d31588e48e99693fded9067cbca23"],["/tags/Mac/index.html","8ae789fb709490a77c2c63227d7d8092"],["/tags/Mac/page/2/index.html","7efd13dca5f17bacd0b3575722a70e90"],["/tags/Maven/index.html","a03f06d2512e9fa326720659b0f002b2"],["/tags/MySQL/index.html","e5ff152a437a6b2ede773bbc2c5a2431"],["/tags/Python/index.html","e806dc81772136b7384ea48ef5a6f241"],["/tags/R语言/index.html","299b10f91a2f6c12faa90e4a9451fd64"],["/tags/Ubuntu/index.html","58073f736d5578d881c0d4c43fbc7c65"],["/tags/Windows/index.html","2ab5dc47066638ea2fd6499d165dcfc5"],["/tags/ZooKeeper/index.html","6d81bdd114142acdc886b393cecb95e7"],["/tags/bfs/index.html","9bb4da36d8c6493160ca287c8a62fd94"],["/tags/dfs/index.html","d72dcc0ee9b2196132e38fb30db9654f"],["/tags/folium/index.html","9a2557efae98b39615a9dd3a0072eb94"],["/tags/git/index.html","3cc92fee20c863c6d8cf68f52221fe48"],["/tags/index.html","bdb623f3a9ca75958226a1b100c557ca"],["/tags/latex/index.html","d380d543e25e43b51c9c5eda08050c0e"],["/tags/二分查找/index.html","7a5d775c9cae0667e210fcd063834949"],["/tags/优化类/index.html","e5680b2ea80a7472fba1af52e6552683"],["/tags/前缀和与差分/index.html","064b44eec4b46d7894f8041b22f9d674"],["/tags/动态规划/index.html","4fef2eae73574f70a773bbbaf57efe0f"],["/tags/动态规划/page/2/index.html","4b9b4d51fb7e80428ad19fcbb6af4714"],["/tags/博客搭建/index.html","08944540784bb1a41d8fde3ca85314b1"],["/tags/图论/index.html","ec0856ed6c9856b12cadc538c5910252"],["/tags/大数据/index.html","96a286f11fa7934ac540aaf4095fdf64"],["/tags/大数据/page/2/index.html","cfefedc011bbf89c77448d9e026ace95"],["/tags/操作系统/index.html","dca7078c1adaa5c914586f22a5881069"],["/tags/数学建模/index.html","c1e2a8ad801a49144f63229f120bfcb0"],["/tags/数据库/index.html","39906957ddda1959113966f50fdce331"],["/tags/数据结构和算法/index.html","19b24efa23f293a589f7a520dcd53e39"],["/tags/数据结构和算法/page/2/index.html","eb33011183d8a08c531a6c2f04185ac2"],["/tags/数据结构和算法/page/3/index.html","509d054adae85a5557ba589a10dcb05b"],["/tags/数组和字符串/index.html","6f302ab5b488e5d5db3e2f395a3d592a"],["/tags/枚举类/index.html","06a434fac6a6aa69c7e9ea0b3f54350f"],["/tags/栈和队列/index.html","1ab560e7e4960211a491f8e2a0a5dc22"],["/tags/树论/index.html","eb77450aafb17575da1cd992be3a6282"],["/tags/测试/index.html","6829ec45b85ac509d329c8318c23f53d"],["/tags/环境/index.html","e95074e905a76b319a7d19ed20ab2cfb"],["/tags/环境变量/index.html","5970961ebd83c582ee582eb874fb9149"],["/tags/绘图/index.html","dc2a53cafceba481ad0e74be5d11626b"],["/tags/编程环境/index.html","46feb78fff70c3eee45c12317811a6c2"],["/tags/网络编程/index.html","445e21202dcd6637e634d7d281b531cf"],["/tags/英语语法/index.html","4cfe63337fb851e57d3690f609e13a83"],["/tags/论文/index.html","d8fe76bad55f9987feb545c7d3943e3f"],["/tags/资源下载/index.html","d1a2984d1df5d0138210e7e1f2536f3b"],["/tags/链表/index.html","b8e43cfa9b08c29cec1f125be3df374c"],["/tags/集合/index.html","2f91cbfc483b85c6e8f718774b6a63ba"],["/tags/集群/index.html","aa53133bf695efcc4efb9323b3969568"]];
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
