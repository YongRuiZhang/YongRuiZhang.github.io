/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","82989b750da00c7f8f97dafcfbb30e3e"],["/about/index.html","dee010e6bc4255b3ff9a1c090b96aa90"],["/archives/2023/01/index.html","36d70ca8214c22d40453493fc4a47099"],["/archives/2023/02/index.html","ed4af5aa5863a8e623384e9490c7a495"],["/archives/2023/02/page/2/index.html","43e9ba81b730b9d31990d2f74f94a295"],["/archives/2023/03/index.html","62b7fb58896f7bd5e51bf20a5dda855f"],["/archives/2023/index.html","2df676d27a539a80504bd5921891195b"],["/archives/2023/page/2/index.html","1bc1bcb6c9d6303955defb7aa5246824"],["/archives/2023/page/3/index.html","4ff095a05de7b80da7c91537e11e621f"],["/archives/2023/page/4/index.html","5eeb92f79d1bd26c4a709349da9285e4"],["/archives/index.html","92571c3f66e7bca44ec251dc0b6ea849"],["/archives/page/2/index.html","daa04a815e3871bc02287d9b520fde50"],["/archives/page/3/index.html","f4065276036e6e9d124e7829c495ca89"],["/archives/page/4/index.html","52c71224e2b97a51dc576eb57fbb5040"],["/categories/Java/index.html","484af74f29492691943cede8ba5a03dd"],["/categories/Java/后端/index.html","ef1ff72588ad8dce4517c2a6928ebf3e"],["/categories/Java/基础/index.html","9c03992fe2d67595a8d2b01a40b756e6"],["/categories/Java/基础/集合/index.html","8af85e987c4a5e5f9819bd965be9c660"],["/categories/Python/index.html","93ccdf8d88375b9aae3e6437c7aeb996"],["/categories/Python/编程环境/index.html","8bbfcfc8ff7ccb314140c8a593ee8ec5"],["/categories/R语言/index.html","2edb86975d6aff62bc7ef9728404250d"],["/categories/R语言/编程环境/index.html","000d9e7a3d95719bc85b48aafc8f9984"],["/categories/index.html","fc68b500dd028f5d749fcc2ef425a33b"],["/categories/大数据开发/ElasticSearch/index.html","47f93cb12b0ca59f8b5f468425a33be6"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","957f1d021be41e65619ad249fe64b62f"],["/categories/大数据开发/HBase/index.html","c872c77671ce1a1a6eb4b823dcc5a821"],["/categories/大数据开发/HBase/环境搭建/index.html","5c88311e63587f0ec3a6c9db6a6b80dc"],["/categories/大数据开发/Hadoop/index.html","a3de3d362c529a0b14f046412020d644"],["/categories/大数据开发/Hadoop/环境搭建/index.html","0dc9550b032141e78dd655cbfa385a83"],["/categories/大数据开发/Zookeeper/index.html","6ea2d1ee2f36a9e7fe71fdd029b6e78b"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","91f0ac5936f7bb5ce5f1b01e271701f1"],["/categories/大数据开发/index.html","888001ad8ffe85ea1ecbae5cc125d068"],["/categories/操作系统/Linux/index.html","7ba757a7a4e7f4413557c8264914c329"],["/categories/操作系统/Mac/index.html","efe792a518749c4aa1f3b0e9e55eed0b"],["/categories/操作系统/Windows/index.html","5b7d81e5f4fcd79ab00d2fcde31ade3e"],["/categories/操作系统/index.html","527f4f427502204e354938a52cfc2aff"],["/categories/数学建模/index.html","a09621ea0e19f5aabf3d58dd9200df76"],["/categories/数学建模/latex/index.html","24fa4f1f77644fa4d7a15cd324255c41"],["/categories/数学建模/优化类/index.html","70525bba2eec8d862adff20d34ca0a89"],["/categories/数学建模/优化类/现代优化算法/index.html","0139c21bea26c31a567c48d3331d13d4"],["/categories/数学建模/优化类/规划类/index.html","5c47a5b28558d2ea08f911736cacfd07"],["/categories/数学建模/绘图/index.html","7f1c6d9704263b07aa801be7403fdf07"],["/categories/数据库/MySQL/index.html","f2065baadb8f531c100c56fa6da01b0f"],["/categories/数据库/index.html","19afb4d12671075b5a9de785b14576d1"],["/categories/数据结构和算法/index.html","f6f924f9b6f197732343e4d02d56e4d5"],["/categories/数据结构和算法/page/2/index.html","df7fbdac2170bda4b6b753b52369000a"],["/categories/数据结构和算法/基本原理/bfs/index.html","77d89c2e90d49f50c2fc13151ef679bc"],["/categories/数据结构和算法/基本原理/dfs/index.html","34d953ccea1fcea15c1a9890bfa29e2d"],["/categories/数据结构和算法/基本原理/index.html","d2be87abc33460cd0ea69b76d39f5303"],["/categories/数据结构和算法/基本原理/动态规划/index.html","585b3a94daeea60d36b68b3007ab85a2"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","f66c8e4492bfc4106627ad7ce3372264"],["/categories/数据结构和算法/基本原理/图论/index.html","87d9074045065bceabbe090114e52df4"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","5ac4111feeb72a742919386d4d02643f"],["/categories/数据结构和算法/基本原理/数论/index.html","d8e75c6e88d86599269f65121958e9c8"],["/categories/数据结构和算法/基本原理/树论/index.html","380d1ef94310cac63c2508152396f345"],["/categories/数据结构和算法/基本原理/链表/index.html","b2035e6be8d9eecc0023c11d70019f3b"],["/categories/数据结构和算法/算法题/index.html","07fa01659e6c0ab124064621b3a8bf63"],["/categories/数据结构和算法/算法题/二分查找/index.html","6a6a3374e2c505d28601ddd162a4d176"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","f8783362a3aad892dcaba28717a8bba2"],["/categories/数据结构和算法/算法题/动态规划/index.html","c80e6eb1d269b2c68f44a508872adf6e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","f205ab903507d18fe8862ccd7ec39f75"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","39b8eb6e93182cb8c1328fe08de5461a"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","d144c62ef0627d46833ad7e8bd4dcd55"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","d517706ac5f773854c1a55480453394c"],["/categories/数据结构和算法/算法题/栈和队列/index.html","23f445bb9837cd60b4aa30fcf76a224d"],["/categories/数据结构和算法/算法题/树论/index.html","23fda061f374a459586da397237aa9db"],["/categories/杂七杂八/index.html","66ff8333fdd4fe9568d34f6f052559ac"],["/categories/杂七杂八/博客搭建/index.html","140e95b1b0d3bdf0ac163eb10ccdc3d0"],["/categories/编程环境/index.html","8bad31aea8eb73241dd7a730bbc29ea3"],["/categories/英语学习/index.html","852be490f8f938bb7338e8b8a6f16359"],["/categories/英语学习/英语语法/index.html","d33e82a1fe76f288ae44e008296d5fe7"],["/comments/index.html","b044ed6f6dba8323ad32a96145a16cff"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","974eceac62aca243e998ff580dcd92ca"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","281575bf87740eb0d1bca5f92da02907"],["/movies/index.html","dc6f070b24b36ee9d03874988c4abc59"],["/music/index.html","e46bb9e1aec018843b973fec48b5f071"],["/page/2/index.html","3e82fef0615aea733a6c43d7c1571b1e"],["/page/3/index.html","b7f3b51aa1a15e135fd015b19e83f610"],["/page/4/index.html","1c7f3a93e414bcddea49919ade4807df"],["/page/5/index.html","c513fb9f8d3b9354cb4f4b39d20682cb"],["/posts/1021360842.html","3e0b5c6dbd53999cba46e9744acdad44"],["/posts/1120620192.html","2e398478b729b7e8ad679bb2ef3759a0"],["/posts/1141628095.html","a056aff5b9f03748c8bc7dd35b8db19b"],["/posts/1168613674.html","ad89d6aafb8b8ea5eea5381e688948af"],["/posts/1219920510.html","095a682d4d905bbcffa7d60959379412"],["/posts/1222166338.html","41f0f96c652e2077396192995c367cec"],["/posts/1259097482.html","8be8dbda7c8595c973100df8cdc02ee4"],["/posts/1271036369.html","687ee66f3d5de8d487de1391974e847c"],["/posts/1312847445.html","eb706c6ab62c8a9805017d979bc67b40"],["/posts/135355774.html","0040da039fd106eaebc47f1179d2d21d"],["/posts/1375344716.html","9a0d6fc1cb24188c98d81b31583cc31a"],["/posts/1388991698.html","f348dc7fb3ed044932060c56461084c7"],["/posts/1410315814.html","6941a25470d782c391b85d27ddd9ddb2"],["/posts/1452790229.html","2fc2cd43948d3ef0c2617b10a27a9d98"],["/posts/1470079884.html","96c54e05752614d80a1a04469bf51dba"],["/posts/1470079885.html","157c8cbb02d01088618bff68642761f8"],["/posts/1470079886.html","0723252729d6f431f26fa7711e733247"],["/posts/1470079887.html","fd793b056eb913c46277412cd50840dd"],["/posts/1498536549.html","1d66511b16252ea4b52680fc209294a5"],["/posts/1557866301.html","bebc83048a77b8319b1f6c1c698cb35c"],["/posts/1571776361.html","4bf824e9213424e848baf7314cf0620a"],["/posts/1605124548.html","c9ce26b4b32632d81057efb8eb3c5f77"],["/posts/1633036852.html","66731d0fe0b122cead974fa9ac7c2167"],["/posts/1765123828.html","476d44bd3d163272804ca501dc95855e"],["/posts/1776114197.html","53f1bf20c412e71b0ed5a161f3b6db04"],["/posts/1817748743.html","3cd8d3c66bac5bde9291f461475dc7aa"],["/posts/1925125395.html","260955da53d3861f944b4740b60b3947"],["/posts/1966191251.html","9c32614135b66c8b79a4b5fcd9367023"],["/posts/1987617322.html","9a896f8830220f6baddb75c044b060ab"],["/posts/1999788039.html","f662fd5c7c57950db90b31022ddb271c"],["/posts/2075104059.html","fabdbbf892ae774e767aa300558155f6"],["/posts/2087796737.html","cf5c13993f82c8c87b3166e7758e080f"],["/posts/2207806286.html","752ee4d6a057341bf933bdeabad78651"],["/posts/2225903441.html","fdc2d346d0bc1c8596800301864bf13f"],["/posts/2265610284.html","6435816f99a6765d879c8512b9c4f534"],["/posts/2281352001.html","ff57d9dea73efd4440ba5c2d522024f6"],["/posts/2364755265.html","cb3a84292c661fb2dfc66de9b597a84e"],["/posts/2414116852.html","668d9ef5d9743568c5828dce19e084d2"],["/posts/2482902029.html","1d293ae06f123f079674de144f66fc06"],["/posts/2495386210.html","9b6dbaba21e181f2782eebfb665d31cc"],["/posts/2516528882.html","7a26124222b086123e65bc6ba5b8d2ae"],["/posts/2526659543.html","3f2e15c02ba23e88834f5253792f83e6"],["/posts/2529807823.html","5e4080149b923ff6bf0c70edfb81ea40"],["/posts/2742438348.html","d019d05b67357892948c2e4c19fa1dce"],["/posts/2888309600.html","d5b8e8662c5a6a106e6ea2821ccbe938"],["/posts/2891591958.html","09c85e7294a05eb44448562be3deda32"],["/posts/2909934084.html","deed14d56ed7519857c8e017e1385dda"],["/posts/3005926051.html","f8291a5dcf5b5e5eb91167cf85781d86"],["/posts/3169224211.html","c6e0fc291b3585aa607a17bb879c6b2d"],["/posts/3259212833.html","50951e1485de787d2c04fdf164dc082c"],["/posts/3266130344.html","d1adbbcc24b49c6f652f992c4aa08026"],["/posts/3306641566.html","e8e1a01bd2f09ddf03c956b07079a870"],["/posts/3312011324.html","518f33ad5f42cda2a3e3b2ed925f9cc0"],["/posts/336911618.html","83e628c9ca0abd308bf108059ada146d"],["/posts/3402121571.html","461719a60dfbaa1421964182edcb488d"],["/posts/3405577485.html","c97cba8eccdc5b3b4bae71ec456103cf"],["/posts/3498516849.html","9cfefd0202d8d7e0906592f46a43ed16"],["/posts/3513711414.html","6a48fdf9a34a9a490bce9b55480fb745"],["/posts/3546711884.html","791c4898f0b89cd18291f465d0cbbd00"],["/posts/3731385230.html","a6bf628fd8c092026f4c25a502c49d9a"],["/posts/3772089482.html","4c1ac5aa7e91461edf7fa8dc1f5bbe5e"],["/posts/386609427.html","6756d029e33497e073790f395812ddef"],["/posts/4044235327.html","5462581fafe1b32cba9de7c849ca0b32"],["/posts/4115971639.html","9c0953721c8a1802bb19f60da9a4c41d"],["/posts/4130790367.html","095a4b0d4695648ea2632fb847e8ac5e"],["/posts/4131986683.html","1344a7b4c621a5a29f59e5403e0530d5"],["/posts/4177218757.html","a18237565acfc3766f848853c2b5b0a9"],["/posts/4192183953.html","26931a4a6e8656534bd8177087c7578e"],["/posts/4261103898.html","3e4de9c14e2c24205367d4a89ebee399"],["/posts/482495853.html","cd4313e94dd13028073fc2c9a816ee3a"],["/posts/488247922.html","f8af270198cac34bedd0528c14bf0959"],["/posts/570165348.html","bccc10ad8e3b221c332602a0ffdc82e1"],["/posts/595890772.html","9ea1fe6ce1f390dadadc62a295d9a83c"],["/posts/694347442.html","2e6f5ef1a37fcd3a596035099358b7a1"],["/posts/707384687.html","d0731d9ee58700dc5d6eab52a06d9d36"],["/posts/71180092.html","65e61787f4b54d31ea2994d2bec996b6"],["/posts/716459272.html","97527ca05b6a3f60cec0a1b7ab987f88"],["/posts/795397410.html","22899f6e1755e46eaff55e3a23109af1"],["/posts/820223701.html","b90975ae93d44e4b66170dc3aae10ec8"],["/posts/830372185.html","6a4b46fe408507037edef9106ce31530"],["/posts/88294277.html","c9d9e3a7afe953506ca95661325fb431"],["/posts/939963535.html","f7f39bea0136293d87261ea3314c18e6"],["/posts/983786067.html","8305393beae7bed48114b8df33606be6"],["/sw-register.js","db634ee9c50e9cf61b7c8f50cbc163b1"],["/tags/C/index.html","3807e3f6b658e3566a164b8f2d88fe83"],["/tags/C/page/2/index.html","beaa231b8c92008785db20b79cb7709a"],["/tags/C/page/3/index.html","9974eeda1ca89430e97dc272bb85481e"],["/tags/ElasticSearch/index.html","8bbaf8b790e4ca25e9491232b62a1242"],["/tags/GUI/index.html","b69701e06aee52d3b0178939e275fa36"],["/tags/HBase/index.html","af385f0e3957bd2bac389f6e4fed026c"],["/tags/Hadoop/index.html","3b30f55e52eb9086fae2c0760c378e5c"],["/tags/Java/index.html","726e1fa3b845a4170f7bc1a6d3135fa3"],["/tags/Java后端/index.html","2e9055ab7f9cd3f87fc396a14da5ec67"],["/tags/Java基础/index.html","52ebef8eef7ac45f1302b33db9ee6e08"],["/tags/Java基础/page/2/index.html","213dbdc8e3dfa620ad853ef19a652950"],["/tags/Kibana/index.html","274f917ad5cd8a9a733cd2a06ba5ea1c"],["/tags/Linux/index.html","be721ec15a7b0f456aecd0e157138e8a"],["/tags/Linux/page/2/index.html","6a6b90712631a685dac8117269954d81"],["/tags/Mac/index.html","c7068493bfeb055414eb16087907e8ae"],["/tags/Mac/page/2/index.html","718bf16301ce6948502e3dc6f77db817"],["/tags/Maven/index.html","5a73783e43e690b71fe92924e92edf0f"],["/tags/MySQL/index.html","3391be13ce1c4776045322ffa6d6d38a"],["/tags/Python/index.html","007711c430ff4f977e29b8713dd5636c"],["/tags/R语言/index.html","918a085e2a88d86fc95a2ac1941ff2aa"],["/tags/Ubuntu/index.html","bf67f42b6d00cbb4df1c60f96c9c8c7e"],["/tags/Windows/index.html","bc35e06a03ab080e0e7e16b2ccb77833"],["/tags/ZooKeeper/index.html","139a5e7f79dc1e7d86f8adddaba30c1a"],["/tags/bfs/index.html","656da33aef2e4e595328ac1e75e77b73"],["/tags/dfs/index.html","d22a7aacf2796d94eb36dd2caf206a5c"],["/tags/folium/index.html","85190980eecb8fdbda441f5094b03c14"],["/tags/git/index.html","dcd496382045fd6b9941fb8679de92dc"],["/tags/index.html","eb2059ba08606b73d8eda131346e1a04"],["/tags/latex/index.html","0a990930018e72042b6b0d97c2d49a94"],["/tags/二分查找/index.html","847f10bf98d54ab88d879232ae982015"],["/tags/优化类/index.html","8b6891dc055f841a7bba6e8e0368233c"],["/tags/前缀和与差分/index.html","9ea5b4d7e302505d0da4f1425f4e8239"],["/tags/动态规划/index.html","2c8a67d875b50bbbd9e728c5bb629f2d"],["/tags/动态规划/page/2/index.html","919b15bcaf658a7a0a244d58121ed2f4"],["/tags/博客搭建/index.html","7b993be0a0d9d6f6752b1b12d329ce2f"],["/tags/图论/index.html","db2d5c7081175b27e9ccfc7d932f85cf"],["/tags/大数据/index.html","e31df1d33da13d9491fbe5eff7ea9d1f"],["/tags/大数据/page/2/index.html","c692eb9317a3aa580e3e36f1070cd846"],["/tags/操作系统/index.html","f3141275a3ce17900f44de2eb23039b5"],["/tags/数学建模/index.html","0b3f89e444268ded97b3c2bfde873885"],["/tags/数据库/index.html","9a9f3297432656bd0830350d1b6059f8"],["/tags/数据结构和算法/index.html","e7a10e413c84561c000905d59c4e132c"],["/tags/数据结构和算法/page/2/index.html","7b992800da395724dcd67ab3d0cca8a6"],["/tags/数据结构和算法/page/3/index.html","1c63b0592edb70f2ed10cffd5b39d73f"],["/tags/数组和字符串/index.html","64eae32367638c4af67382224e720e68"],["/tags/枚举类/index.html","93554c12a06f1472ef7bdfecbb3d5b46"],["/tags/栈和队列/index.html","15052126191f4e00b85de5da2b1cee22"],["/tags/树论/index.html","7e603ab4620e3fbcde87880583dfbb52"],["/tags/测试/index.html","1c4377acddf2fdcfd5efebea9a08805d"],["/tags/环境/index.html","9b91ae1efd20acc0f2c1f03ae60e38d4"],["/tags/环境变量/index.html","61dd6dc3683b5e623dd11963b3785628"],["/tags/绘图/index.html","92b7491af4fd65681f115fb5e4147549"],["/tags/编程环境/index.html","91a8e6fce1ac38ac12c4e0ab22124527"],["/tags/网络编程/index.html","d763d3dfdec6acd1d174ad8547024d86"],["/tags/英语语法/index.html","194e9b8e0801ca7bb1f30a43e2ec502c"],["/tags/论文/index.html","14d522639856f93e544fce1dba5c709e"],["/tags/资源下载/index.html","1f98681e541b59b2efff656fb495a6a3"],["/tags/链表/index.html","fe738626b44125ec49e00a116ebaac70"],["/tags/集合/index.html","eab7784adeda1e9fc0d36274a0de6abe"],["/tags/集群/index.html","4922e947ea1d319da659293c9b6f9b68"]];
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
