/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","5e4aae2423daaf4992bec1b8a7b16156"],["/about/index.html","a5a807a98aa289df4d7439a944641b63"],["/archives/2023/01/index.html","d4d04c4bbb2c45a62a89e8f77412e4b4"],["/archives/2023/02/index.html","3ef80088d55b0e94251a85c59820005d"],["/archives/2023/02/page/2/index.html","7973bd5b527ab30e032d1bda78568f9c"],["/archives/2023/03/index.html","33a634065001917e4db17b2d9ea8462f"],["/archives/2023/index.html","4ebd960637fe5375a6d74b241a6250a0"],["/archives/2023/page/2/index.html","3a5dae68d1257bd4f15709b250aca13e"],["/archives/2023/page/3/index.html","2dd5019246bc430ab3741438dfffe9f8"],["/archives/2023/page/4/index.html","97476e380f42a6f38079b791445db347"],["/archives/index.html","27dbe542ba022e89fd215d8af1b761e6"],["/archives/page/2/index.html","e61d73e0d23ccd99f439062082e3ecc7"],["/archives/page/3/index.html","9219cbb3e5e8fa77fa7d8f813ff5162e"],["/archives/page/4/index.html","2cb27d22880c33fa02a944505bf40079"],["/categories/Java/index.html","9240b4144e8b9ce15dedf7bdfebb4abc"],["/categories/Java/后端/index.html","b4cab469435f18847ad7b8c564379353"],["/categories/Java/基础/index.html","882c43157eadc6ebd6073827d5c76d48"],["/categories/Java/基础/集合/index.html","db5b551b1e47eb9ce82dbffdcecdbc5b"],["/categories/Python/index.html","2b825cbe6d9d1c54fe7fce38b7514c92"],["/categories/Python/编程环境/index.html","705eea2da0c8c0a2a0f460b1cc8e0cbe"],["/categories/R语言/index.html","97480a4bf51d9cd78fc6ed69fcbe4edd"],["/categories/R语言/编程环境/index.html","d5574a18bcc84e41acd470d2ae10cbc2"],["/categories/index.html","2ff2c0a7198cf183677f22c7a2fd92a3"],["/categories/大数据开发/ElasticSearch/index.html","4695000374432d0702c41864abbe01f2"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","9acda24b4566cac17a6add9c723f29a8"],["/categories/大数据开发/HBase/index.html","569490ed4ccb6fbb86afc26f7c67a7a8"],["/categories/大数据开发/HBase/环境搭建/index.html","9bf7ba03f381359561342e172a0f1f46"],["/categories/大数据开发/Hadoop/index.html","a5d1da3be5a07724160874e6b54fbf74"],["/categories/大数据开发/Hadoop/环境搭建/index.html","53bc434ef2740d9692f20f1900b02027"],["/categories/大数据开发/Zookeeper/index.html","c505e57eb2108f8c60e549bb6fda5993"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","d92ed2ee302d9c41ca2340f0e8b9c71d"],["/categories/大数据开发/index.html","f5181acfdabb429c0c19a8e73eefc84c"],["/categories/操作系统/Linux/index.html","1da01b82bf147538f593d5780f321338"],["/categories/操作系统/Mac/index.html","9cb967bb57983a1094ddf3c9dc87b301"],["/categories/操作系统/Windows/index.html","7a28b896c1b2d4f01da12dcbb06f0ac4"],["/categories/操作系统/index.html","92234f2f91ab85756c8e20a9414731ef"],["/categories/数学建模/index.html","71980afcfeab77d9db61ffe8de82ded8"],["/categories/数学建模/latex/index.html","34498c8cf10ef9ecd64d7de2d48e9719"],["/categories/数学建模/优化类/index.html","2dcdc3007f08d274c10619e47fe34c2f"],["/categories/数学建模/优化类/现代优化算法/index.html","cecd11d72f60e8441299fd452cee4722"],["/categories/数学建模/优化类/规划类/index.html","86d2f49e6396221f3bed2d2fd68db84b"],["/categories/数学建模/绘图/index.html","3c5fa3f57d7ffb6eb441a1bb1c100984"],["/categories/数据库/MySQL/index.html","27812101a417ed6c0794be5f0457d3ba"],["/categories/数据库/index.html","8cb765c071812180183a70cb4bf66954"],["/categories/数据结构和算法/index.html","c6e8aabab4fc866e94dc86842145a7f8"],["/categories/数据结构和算法/page/2/index.html","d80e6c5dc993443ca7d4fad5d06fe24b"],["/categories/数据结构和算法/基本原理/bfs/index.html","f47262159e6a6a592be8b152c9446982"],["/categories/数据结构和算法/基本原理/dfs/index.html","05fcc64969e630a7c42e800c0b7703fe"],["/categories/数据结构和算法/基本原理/index.html","31007f8c28c8c9a5c1da3f0717d2a517"],["/categories/数据结构和算法/基本原理/动态规划/index.html","9d4871615704d3778e6d426529573439"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","100a245a2a3cfdc1180cb360cd74d6e8"],["/categories/数据结构和算法/基本原理/图论/index.html","08c0e1c817166397faee7ab5df30b844"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","10e2f8d11b7ee635a37f78c51635de99"],["/categories/数据结构和算法/基本原理/数论/index.html","8d8aba51e681330d08e960c2776ceb87"],["/categories/数据结构和算法/基本原理/树论/index.html","b1bbfd31065b35b3828963d2f2f67def"],["/categories/数据结构和算法/基本原理/链表/index.html","41c3c0a1f832bb6b5ac45dc44311773f"],["/categories/数据结构和算法/算法题/index.html","44a3b59120d30056cc9cc37d022efa8b"],["/categories/数据结构和算法/算法题/二分查找/index.html","c868f35307f56c529bc0e9a46fdd9134"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","279846a475be1d90fe01049692218e52"],["/categories/数据结构和算法/算法题/动态规划/index.html","f57b61d96acea1841b7e4b566da64350"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","aa63749561cf26da87630dc4f086f374"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5ba7b9c52984f9e4091c34fe375fc566"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","695b7c55bb063c8062a68d47a72f8e5f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","51f32702e3ec9cfcc202e44a06fa5b55"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8f4164a2270b2fca5dbdaaae8be5db7f"],["/categories/数据结构和算法/算法题/树论/index.html","67bbb4738b5cfbc33b7c1068bc285e7d"],["/categories/杂七杂八/index.html","2c9a4674bfa8708ff3b9cb77f09906a2"],["/categories/杂七杂八/博客搭建/index.html","18df8cb92a2940bd88e4c8a59e6e9320"],["/categories/编程环境/index.html","2b0e32ee843b62e95da037a8a2e90bb5"],["/categories/英语学习/index.html","ad00def931ba79a32f982dbb57dba6bc"],["/categories/英语学习/英语语法/index.html","7a4343c9eb92f126a84c22fe8fa8f325"],["/comments/index.html","c0cc8171af26dc1f483a7fa25a83e375"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","484c1ef2958d244edb156585afa57431"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","629462f9f8a44a427a08736694c4ef1c"],["/movies/index.html","2dcf071d89c35f11b8d3407b56c2b9c2"],["/music/index.html","3b6262dd180cae3189db1acfa891831b"],["/page/2/index.html","3f17e315fc4889dc1e17d07b1f44de99"],["/page/3/index.html","fe1bf7b59d4883b6d129944aa33a27a1"],["/page/4/index.html","7e43662cb00ea60040a6a12ce366d5a9"],["/page/5/index.html","6885737b77f768fdb8b17dd093daba3b"],["/posts/1021360842.html","414e250a495efce84b64a401554db3c3"],["/posts/1120620192.html","e6fd52bbccc0d288fc1d3ca961f7cbe6"],["/posts/1141628095.html","fac990e77b1588a272a506279aad51e3"],["/posts/1168613674.html","2deaaf744037d8ff9ad82c3d0200482e"],["/posts/1219920510.html","abfe697cdcd9fc3c587e7d8727b70fde"],["/posts/1222166338.html","8b3e5cbe1b9e663b0727fc3d24cf02c1"],["/posts/1259097482.html","8bbfe948b7e9c06fb17f386b28550cbf"],["/posts/1271036369.html","dc67b14c2aa5eca86d707e72b2d571f5"],["/posts/1312847445.html","c3af895afc75687a18064540618e0777"],["/posts/135355774.html","d9e308c35698a68de878da05df63f657"],["/posts/1375344716.html","a253fd265261632698043393f08f283e"],["/posts/1388991698.html","239769d65fbdc7452cc16b6d97c8323d"],["/posts/1410315814.html","72e97faf70fbb11f1c832d73c60a647d"],["/posts/1452790229.html","130869fa1f0e0e8feb7b41c530d8851e"],["/posts/1470079884.html","0b42a4d5eb15b8c7e93e71854b49d21d"],["/posts/1470079885.html","5e39b4d6d2d3fe92cead910934b02e17"],["/posts/1470079886.html","4adb2ed4726f90b38d765027fd186b8e"],["/posts/1470079887.html","7dc461ee84a767128c4dfc4bd86ef05b"],["/posts/1498536549.html","c43d9dcc2795367b1235ab1866954ea0"],["/posts/1557866301.html","81d986cecd34b0cbc4936dd03df56f9d"],["/posts/1571776361.html","a1c8576bf6edbbc8f951e1a038760254"],["/posts/1605124548.html","036c156a45120fc48698898b578999eb"],["/posts/1633036852.html","f43f34cd78ac48b52b5de2b4ec964a4b"],["/posts/1765123828.html","ca9991b87b32dfe93132541bba12c4bc"],["/posts/1776114197.html","8ec6bce56b56a2a0e700bbe735d17e05"],["/posts/1817748743.html","61c44d562816ffb7aea14b6f3cd917ef"],["/posts/1925125395.html","d35ce0e0957585ff8c566b7884ec4298"],["/posts/1966191251.html","c35989745ef1506f68efd1e44cba5e4a"],["/posts/1987617322.html","8e9ec11138213e899f7528d6b2236906"],["/posts/1999788039.html","32f30d1fb44a6ccf2146f33c8ecd23cf"],["/posts/2075104059.html","bf4d7aab988e91c71ee7a7f9989640c7"],["/posts/2087796737.html","aa24734553f25051d0e60fbce6f1001b"],["/posts/2207806286.html","9c251ef5888ff59eb715f49d7fb044fa"],["/posts/2225903441.html","5e5a53582299abfb6c1b4442942c7db2"],["/posts/2265610284.html","e8d93b9b813b6d52c8d59edef4f10294"],["/posts/2281352001.html","3510715043420b1c78f0abfe81846c48"],["/posts/2364755265.html","42409030a4e44e29b0dcfb1a19a7caed"],["/posts/2414116852.html","563fa2bf1cc92aedd4cdedf7085e1eb3"],["/posts/2482902029.html","280a9865d1eea95d9d3db25d9a00084a"],["/posts/2495386210.html","46d9a30130a6784f0f1c287212531435"],["/posts/2516528882.html","4c449f2e7f9f9b6b95163e07cbed1f72"],["/posts/2526659543.html","1b8cc52de58c0e47041646ce33589415"],["/posts/2529807823.html","ae56a5afe027c7ec49f85959b1f835b0"],["/posts/2742438348.html","907cfd482aa7a9b442c2046fa8ce9e0b"],["/posts/2888309600.html","c2d2db441379e395d9df9474b2a84c46"],["/posts/2891591958.html","cbd77eebfe1ba499a22eef7090b32aee"],["/posts/2909934084.html","f7fa92c1502d96c05c30a6ad53a3d5f9"],["/posts/3005926051.html","9d0fce37b7b14262eba777dfbade9b3b"],["/posts/3169224211.html","6ec18e4f45502e981507753de2cf864e"],["/posts/3259212833.html","7b1526e7ff307629b482332e5ffb6ec0"],["/posts/3266130344.html","815512ddead1fe8a1f337bb31f058a48"],["/posts/3306641566.html","f4da1c5c94c45650e100a0b78a5910c1"],["/posts/3312011324.html","9fd24ec9eef0faad29a6e186c8879242"],["/posts/336911618.html","7a1eae14db21a7db5f3cf730608f36c6"],["/posts/3402121571.html","021127186a19e83f32d17fd139264e51"],["/posts/3405577485.html","6351cc88d31617da77759c34f9096e84"],["/posts/3498516849.html","70a7a0f82dc6c92e0e67c2206844cbaa"],["/posts/3513711414.html","9e5410fa840e83c91a2f1634ebebbb84"],["/posts/3546711884.html","c22bb5f5f28251a78311c62f766aa985"],["/posts/3731385230.html","656de330060b69da962703e72ec7ebd8"],["/posts/3772089482.html","b7a0ff01580bd1b112c6454779747978"],["/posts/386609427.html","ab8b87cdf4c54c9684e89b96b4694fac"],["/posts/4044235327.html","0dd3a70aa5da28fb9e1231e52432b801"],["/posts/4115971639.html","14ac1260c304636b23e155298e57691f"],["/posts/4130790367.html","5453a68d17b4bae0e14773b9ba91cf39"],["/posts/4131986683.html","c32a8c4bc54cb1d130344b8f9d8b5c32"],["/posts/4177218757.html","72b6ceeb46929850ef9269a7a314074b"],["/posts/4192183953.html","4afc124ee5a79dff3e5dc0310bc03280"],["/posts/4261103898.html","1c9b7c28f42b31600d56a83c239c25fe"],["/posts/482495853.html","d385038e53de256077dd7cb5367c2de5"],["/posts/488247922.html","4e5755b855199c466e17c3c65d5a7b1b"],["/posts/570165348.html","b65c36ed5335b8dc96701712f0d42347"],["/posts/595890772.html","df6e6de1733f08e231e8072d3f570ae4"],["/posts/694347442.html","d54b623a72dab7e1e522ce0348c09935"],["/posts/707384687.html","713c5dcad003f074bdd29739f15b79b4"],["/posts/71180092.html","c212de92c725385f987f8024568d5e83"],["/posts/716459272.html","68db9f6641d837b2def0d9bc7ed21d17"],["/posts/795397410.html","bacd8a7201e02ef2bcb89fe8d4109e2c"],["/posts/820223701.html","5af80f59e5c1f622bc921d96403db90a"],["/posts/830372185.html","d120c9b2db0789a086f2af62d1e4ad90"],["/posts/88294277.html","ee49a8175a056c9c2cae48dda1731455"],["/posts/939963535.html","ce6bd64429d6d8dc20084b012e4d5943"],["/posts/983786067.html","50a533ef4dbf095054969658877e96b4"],["/sw-register.js","f11dfc3662cd6c4c0b2b6c4f38b20b27"],["/tags/C/index.html","e8f2f1371108aca57cb752db9fbd07ae"],["/tags/C/page/2/index.html","89e03f591651e896a146ea99576f1cf3"],["/tags/C/page/3/index.html","a3196e1a597502cc538eba0bee037107"],["/tags/ElasticSearch/index.html","316fd282fb065e19a72c1fe6b852e2d9"],["/tags/GUI/index.html","baa2b5f9a7ae416ae7712f8b68b29f63"],["/tags/HBase/index.html","7ae6c94bcd69a5f222b8c0b03fc37f3e"],["/tags/Hadoop/index.html","e15409e0b15f4dd106d5ceb5d6064dc6"],["/tags/Java/index.html","dc44b62790b680a95a619c26640ab2ad"],["/tags/Java后端/index.html","5b73a9c54316515c9a6817e65407d406"],["/tags/Java基础/index.html","ade48ef8d48cb4e7d15f3aac12257230"],["/tags/Java基础/page/2/index.html","c6c321be66c348bb044f31874210df3d"],["/tags/Kibana/index.html","31880191846fd206804d90d7735b3621"],["/tags/Linux/index.html","4b4a159886447a94a4e2d11478578867"],["/tags/Linux/page/2/index.html","c3848d656025cccbe6d5535db613897a"],["/tags/Mac/index.html","a9bfee5888e8b925d74e271143473099"],["/tags/Mac/page/2/index.html","709a62e4765fbb7ac15faea625b1dd4c"],["/tags/Maven/index.html","db15b885c8a426562ebf310a31d4a19e"],["/tags/MySQL/index.html","a1fa044ba8e69f2913e98468cccf7d6d"],["/tags/Python/index.html","51f1845d4aa556dc841294321ca2e02e"],["/tags/R语言/index.html","a1e3a84360a8d7ba706b49e43f570d9a"],["/tags/Ubuntu/index.html","9dde0622d28c877b150b3b01d6f03b49"],["/tags/Windows/index.html","e8e68585420d456dfbf6ad5b903f5d82"],["/tags/ZooKeeper/index.html","ee2c0616b4503534c1513f5149598a48"],["/tags/bfs/index.html","48971c4e484e7e6a9d245fd1b8920b46"],["/tags/dfs/index.html","62edda773efc1b2ea5fe7b236928200a"],["/tags/folium/index.html","2f0fa52c797683f969d15314c378ab79"],["/tags/git/index.html","5b5092084d45910cebaf847055d32ed2"],["/tags/index.html","c0468fa0bfdbcec0ef63a7b182b1e335"],["/tags/latex/index.html","7940048ee8d72b603ac9a6b4975770ac"],["/tags/二分查找/index.html","cc7bd67740e03e2a950bf4d04ab76a9c"],["/tags/优化类/index.html","b9d9136a2e8ffd257498aff70293cd0e"],["/tags/前缀和与差分/index.html","81e60c0db227e5892418b17876d264ef"],["/tags/动态规划/index.html","cc6de2b12fff77c3990b4e5df0f4445c"],["/tags/动态规划/page/2/index.html","abfa124cf6c66811b9b71fab9a06b090"],["/tags/博客搭建/index.html","6ef4af54a6dfdfe68e1aa06f606bad0c"],["/tags/图论/index.html","b5051144a321abe04f996d3a52f8befa"],["/tags/大数据/index.html","940f7477c7d382331144bd16b7b3f96c"],["/tags/大数据/page/2/index.html","ee22b8269ac976f40dbc552ca739e170"],["/tags/操作系统/index.html","b37c983bce7584a3a360b6b4833f5782"],["/tags/数学建模/index.html","e9c7efeea9c8150ceb0b88397d70a970"],["/tags/数据库/index.html","b0b76924d79e04532e4415373602d5b8"],["/tags/数据结构和算法/index.html","f2cd322fd5d6f7a282a30c0122e0b7ea"],["/tags/数据结构和算法/page/2/index.html","384de2cd4e7b71d4627d7e51802ab756"],["/tags/数据结构和算法/page/3/index.html","a49b18a4da199548094edda25d6f2a11"],["/tags/数组和字符串/index.html","aee3a1ab84519f89c21e6e63f61f032f"],["/tags/枚举类/index.html","377db054c0da3a1968f58db26c1515f1"],["/tags/栈和队列/index.html","408c1b8ab9052e9ec2612295a455dff0"],["/tags/树论/index.html","1b1f503bf348b13db0ed946b13a259f3"],["/tags/测试/index.html","db7a75ad84eb71116f57162e3a34c8f7"],["/tags/环境/index.html","fec90942034bf227ba30b09891695f04"],["/tags/环境变量/index.html","58d4fdff78432455939bf0bbb5ce9516"],["/tags/绘图/index.html","fca4a4a507529e5ba02e86efd284a569"],["/tags/编程环境/index.html","86b4281f74a97db6710e31413217b841"],["/tags/网络编程/index.html","ccd31898e62aa1781085f01adfb3c37b"],["/tags/英语语法/index.html","4283d9c9c998bbd49db0eba6cb140ddf"],["/tags/论文/index.html","fca478c5392fd418daa36e4746412876"],["/tags/资源下载/index.html","70f21eb8af8f4db1ee08b27a6c0eecf3"],["/tags/链表/index.html","97d8072260638832f1de36c547cdae97"],["/tags/集合/index.html","cece9210f7a0e585717104611412bf2b"],["/tags/集群/index.html","fa86118f7fdef9c4a89b94a8b2bcce48"]];
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
