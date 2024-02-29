/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","b4dc24be3a45fd5b83e255098472ecd4"],["/about/index.html","8b27792c62c63312f6cda047e50d3e40"],["/archives/2023/01/index.html","e9fe06c935bd4bf9934664961407fdea"],["/archives/2023/02/index.html","f52133510a87bd3cd136d96b0283f362"],["/archives/2023/02/page/2/index.html","bfe4470ffe58a393e00b2fe063f1c3b5"],["/archives/2023/02/page/3/index.html","e7e0b83ee0b257bfd355a6f819771810"],["/archives/2023/03/index.html","08a22c402713982f60a3ee4f436a7411"],["/archives/2023/05/index.html","32191f21537a656766eb6c2336c87170"],["/archives/2023/06/index.html","50fbda1826ca65748dff6a0fb1f8d798"],["/archives/2023/09/index.html","ea57fc22c6b309eb8dfab3b3047c45d2"],["/archives/2023/11/index.html","6182125a1187f3a3bea379b726606415"],["/archives/2023/12/index.html","ea2cd691ee5da6fd8ddafb13f596570d"],["/archives/2023/index.html","ddf6bd2fb2510272da0cf973f74f6386"],["/archives/2023/page/2/index.html","399d490f214343a666e58e4a5a814f30"],["/archives/2023/page/3/index.html","28c7bb589da73a806b621c950d13b805"],["/archives/2023/page/4/index.html","705802014fba45d026eca8e67b90d6f0"],["/archives/2023/page/5/index.html","1639d43ea00406d4ea8d07e35889ca91"],["/archives/2024/02/index.html","c7715b21dabbc1e3776c8e876fab2f01"],["/archives/2024/index.html","c3b5d50b6c901f889a2ae2d7116b7f02"],["/archives/index.html","d89037ea922233f4b54fa64532bd007d"],["/archives/page/2/index.html","dfaa432d6db5dca562149dcfb212dcdb"],["/archives/page/3/index.html","5506aac3b33ad768de286ebfa68af91e"],["/archives/page/4/index.html","1efb8feccdf6d1cb491122286495e12d"],["/archives/page/5/index.html","0b7bbc0f16912a95017eac963937e565"],["/baidu_verify_codeva-qQP2iZOMLX.html","d61bb0d3d1c431ff1bc0ccea6003aeb4"],["/categories/Java/index.html","1532184db145402f2759c57824820da7"],["/categories/Java/后端/index.html","e03e2279e585d0259c34dae5704d8eb6"],["/categories/Java/基础/index.html","f168ea23808346407da08e11700ba184"],["/categories/Java/基础/集合/index.html","c365278fc7736d91bfd35b96fc4d9bd9"],["/categories/Python/index.html","c5e72d7bcc62b1583dd883118dde59f8"],["/categories/Python/编程环境/index.html","b9a3e0dd9f5ef19cb4916c93bdf53f4f"],["/categories/R语言/index.html","a953a12c5845e14a12181b9f1b293662"],["/categories/R语言/编程环境/index.html","ca81d99c42fa6ba6003886ec86c4afc0"],["/categories/iPad/index.html","930d2fa4901a2d0c91fc62f4b440753f"],["/categories/index.html","4f22613fc0ee8d04779ffb94c70ba029"],["/categories/中间件/index.html","8db3476d44617701eff4a8fe4d3e2831"],["/categories/前端/Vue/index.html","21621b451aebeb9370d14cc356082d05"],["/categories/前端/index.html","2e2b32ea949346f332eade79f4c36198"],["/categories/大数据开发/ElasticSearch/index.html","8a59ce4b15a6bbf3d9539f634d69e3ee"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","3711c937aaa0c32171abb6d6626adb0d"],["/categories/大数据开发/HBase/index.html","fb97286bbbbf3c3fb727b74693a2f4b6"],["/categories/大数据开发/HBase/学习笔记/index.html","425278a67424f119171d3bf05ae250bd"],["/categories/大数据开发/HBase/环境搭建/index.html","4ee3491a72de2a0bc4923f10ab3e7e8c"],["/categories/大数据开发/Hadoop/index.html","9447f7aefd99935b345e0a9c6afb76f0"],["/categories/大数据开发/Hadoop/技术/index.html","f23e5aaa7a099c0553c0b9bd13e9e2d8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","c5dfd367b8a6c719d5fc3f5a50d1859f"],["/categories/大数据开发/Redis/index.html","df77ee876016abc943904e63f7deae03"],["/categories/大数据开发/Redis/技术/index.html","a2856a663b101f50624b718771199746"],["/categories/大数据开发/Redis/环境搭建/index.html","7c2198abf1b734e50cee15106aaa572a"],["/categories/大数据开发/Spark/index.html","091498b801e836dcc6fe954480bf4cbd"],["/categories/大数据开发/Spark/环境搭建/index.html","46b05bd7cee59edcc745b51c9c0b8d65"],["/categories/大数据开发/Zookeeper/index.html","a4dc750bb6f533354a9589f71063fc8a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","8c38ebb31229615e3a17312cbe2c2fef"],["/categories/大数据开发/index.html","15f9fa59f9fdfe943e0456829f6b0fe1"],["/categories/学校课程/index.html","6a96e747c2560f78be7c62f205437e7f"],["/categories/学校课程/计算机操作系统/index.html","375bb97f6180aad1c1bb49e643f9b4dc"],["/categories/操作系统/Linux/index.html","5fcaf545fa99f59b73c45f3d5ffe3846"],["/categories/操作系统/Mac/index.html","efd6b3f6b00968059e163162d695b8c0"],["/categories/操作系统/Windows/index.html","b1e884c38263be60bd3988e9cd32efff"],["/categories/操作系统/index.html","8f070c0ea629384cd4113b97499694ce"],["/categories/数学建模/index.html","16466b53a563db4fb4281d0f241d7c3a"],["/categories/数学建模/latex/index.html","ed314cb5c5fffe23ac6c5c15fd9c49a6"],["/categories/数学建模/优化类/index.html","b6893bfcb539027124f44f5022699472"],["/categories/数学建模/优化类/现代优化算法/index.html","40824b2f2b32fb9edb3753b98ac5f8be"],["/categories/数学建模/优化类/规划类/index.html","75b21ea3d90f78be31d7766dacc448aa"],["/categories/数学建模/绘图/index.html","b7f4458392bdd04422ab993e3ed5a1cb"],["/categories/数据库/MySQL/index.html","b04bdf49643048762caf8ccaf0dba7f2"],["/categories/数据库/index.html","23972db47426ca540704a672975fae09"],["/categories/数据结构和算法/index.html","33f9f4021d654f681e18931244b3c136"],["/categories/数据结构和算法/page/2/index.html","88ecfc0a0949a512ceee365cac06b3ab"],["/categories/数据结构和算法/基本原理/bfs/index.html","423f817de942d6980b123524e8fdd8d5"],["/categories/数据结构和算法/基本原理/dfs/index.html","99db981e163baf1f9fe1ccceb9d84ce8"],["/categories/数据结构和算法/基本原理/index.html","80a8bbdec534912cb895d8cb8c02d4a2"],["/categories/数据结构和算法/基本原理/动态规划/index.html","0d4e25c6bb7389084aba91dc274bd020"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5aea354ff38684434c8dea8c097bade9"],["/categories/数据结构和算法/基本原理/图论/index.html","cf16cf0bb0ea2059c9c0dbda8b5a9ded"],["/categories/数据结构和算法/基本原理/字符串/index.html","ae741366297661c64a6651d715953732"],["/categories/数据结构和算法/基本原理/排序/index.html","b901db14c9d58b92b7fdd9d5ef3114be"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","5e645a885df6095177dd6928d9c4a116"],["/categories/数据结构和算法/基本原理/数论/index.html","dc6efd59d8d4b78fc3ab7684e7c42509"],["/categories/数据结构和算法/基本原理/树论/index.html","1acff967a867c6e50ecba9f95a941687"],["/categories/数据结构和算法/基本原理/链表/index.html","bddba11950f14af7e0ce48b724e9169d"],["/categories/数据结构和算法/算法题/index.html","351d17918d5f423f97a242bb87045edd"],["/categories/数据结构和算法/算法题/二分查找/index.html","6447c2e1e76f78f8309aab670f93e066"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","b5fe49ce2e7c275ca6f99d168a798677"],["/categories/数据结构和算法/算法题/动态规划/index.html","ecfac21d708078e8933f8c9118d241d4"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","5223f6a2c99136ef480da49b121defc1"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","80bcd9f0c2178bdde142d62674711699"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","b55b2716cf502053981d7ea73e34cc63"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","19829ad1cfbb14104d8fd386cb27c8ac"],["/categories/数据结构和算法/算法题/数论/index.html","5d90f5612ce4ade295ce9f9a202eb662"],["/categories/数据结构和算法/算法题/栈和队列/index.html","a53223bec744471b0c1e8611c3f7ce2c"],["/categories/数据结构和算法/算法题/树论/index.html","db95680c785ad4bf5fda1e908d9c32e4"],["/categories/杂七杂八/index.html","26f876088b906796f32a8b20d79d6c22"],["/categories/杂七杂八/博客搭建/index.html","dd73513cb0d77f9b88e3d702c1ac9054"],["/categories/编程工具下载/index.html","0948617a9dae7a4765bc55afe07c58e7"],["/categories/编程环境/index.html","afd73557d12dd515b54fae539ab1e8f2"],["/categories/编程环境/大数据/index.html","30355363d34b82dcb4ad076c68234ab4"],["/categories/英语学习/index.html","4ce86a54d28ecf401a29b8a9b00a6135"],["/categories/英语学习/英语语法/index.html","56fcb367435fc759ad455708e0ca1137"],["/comments/index.html","9591971a876b5448d78300d4e6241d7d"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","d8be05a6fbec44b7b035d7566254ff3e"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","221d3b73b27861e60007bd01d8bdf23b"],["/movies/index.html","d05011c172463955723f15d102e4d2c1"],["/music/index.html","5b3ebe5dd726830da44c66b2a0fb836f"],["/page/2/index.html","7d4335b30ed0c8a1171d27a1e6eb45fb"],["/page/3/index.html","75ff7ea91024c08f6c3a2435271c66cc"],["/page/4/index.html","3ed74f433562a63e3175f6196ddfe965"],["/page/5/index.html","f7fd6345d7d4d3dbb34df5277c02b467"],["/page/6/index.html","0ebe4e89b36e6421b411c97b41f2a5a5"],["/page/7/index.html","f5b00cc71e40698d701f586382ba3dbd"],["/posts/1021360842.html","250bf81df1b21df380a555588489799d"],["/posts/1120620192.html","68d734c7c4a6b74f81e6cae7a8d2862a"],["/posts/1137707673.html","fed8b9e7569fe7657cb2f86b26212bdb"],["/posts/1141628095.html","3996dfd4d0b82bbf86938e30bad74516"],["/posts/1168613674.html","3c9ff76bb4a5b103445cf0716b359e41"],["/posts/1219920510.html","87fe27b813bfff5d0e7b80bd659dabda"],["/posts/1222166338.html","e7b081e28e54e5993f319097206de622"],["/posts/1259097482.html","e13e6de83b24e6664aa7fa11d8c09c3e"],["/posts/1271036369.html","a851d2be01c1f52b476596294ed0fa4e"],["/posts/1312847445.html","3ec1b409be726b2486ea690b0f110559"],["/posts/135355774.html","b5da359c31062ea5d2fa28fd44e56778"],["/posts/1375344716.html","3c62550c09db84d57788ab720565891c"],["/posts/1388991698.html","ea152d82c972e18463140f47538817aa"],["/posts/1410315814.html","45a8e81f8a9cf4db15971809efcff52e"],["/posts/1452790229.html","5d91568b03e67c3049137adee0cd1280"],["/posts/1470079884.html","6cc858ef9b382d553f0b59cc04c5fa46"],["/posts/1470079885.html","f9cbdf58f2d8557747d4ab47f6f20885"],["/posts/1470079886.html","c24f1f6ffbed8fa1687b3ca01067a69e"],["/posts/1470079887.html","44339670e5c28e26021e4fbb38f68516"],["/posts/1498536549.html","699b5702222308eacc57f879fee1bb32"],["/posts/1539568593.html","9e7db0d3c918de4dcbbdfb8206ef8dba"],["/posts/1547067935.html","3e65e485d8a39dfbbe8d5b92184bdd8f"],["/posts/1557866301.html","6a1f55087a15da9611bef412474a20f9"],["/posts/1571776361.html","e38c01bad5aa32dd4793ca411e8d9fe8"],["/posts/1605124548.html","283581a972365d3231c7e36934d0ebd3"],["/posts/1633036852.html","bd19e37bf93b73db4bc8252955a8701f"],["/posts/1667740714.html","8aa14b5e228e114358339bc2496a8366"],["/posts/1674202625.html","5ff4689d6fc1eb7ffc77eba71f09189d"],["/posts/1765123828.html","61ef76b426d9baf5d424b50983f01cf8"],["/posts/1767336200.html","053e28364debd41b985d636c902d0f24"],["/posts/1776114197.html","1df75a60c8cf278ec5faa5cff69fcf01"],["/posts/1817748743.html","e33ff6bd2634961fa8df156e638f472c"],["/posts/1925125395.html","197815b50fa514968c58b5e6d434910a"],["/posts/1966191251.html","a62b9044467036639844e2ad7f8cc411"],["/posts/1987617322.html","98b63cb65cfefa51f9e6eed92e789e01"],["/posts/1999788039.html","c0e373ad56aea76ec4667cb6e78367fc"],["/posts/2075104059.html","1a71d73966dbd3e17ad82c0301df9bad"],["/posts/2087796737.html","70cf7243674337e8862d1307a65466ce"],["/posts/2106547339.html","f5d8db3ee28db92adb79363ccf6c4799"],["/posts/2207806286.html","78f72ff11dd3379656f47222053f0cf2"],["/posts/2225903441.html","a0af27241a82bd40174845af5a419fc1"],["/posts/2265610284.html","720b2794abd3d4a2d8325ba001058902"],["/posts/2281352001.html","766fdcfb9f9cd2a5572db744c037c4bf"],["/posts/2364755265.html","499a3b21d72fb35e0d0f961634cbd8b7"],["/posts/2414116852.html","c804aa65b71ac303a1029c929c3aa281"],["/posts/2421785022.html","5d6889aef87df6fdb18844e6de8ba3c8"],["/posts/2482902029.html","7ac20026ec102796cff3718a0cfae65c"],["/posts/2495386210.html","7fca766e63ff0a51e6977de5a194316c"],["/posts/2516528882.html","c05dee7479e0b8c57c76ced0c8214b90"],["/posts/2522177458.html","07261fdc6cfd18fe84752fb69ab35e6c"],["/posts/2526659543.html","e65a0fddda5159d95ef1dcb2a07ed17f"],["/posts/2529807823.html","e814a23a8c1b2a9410076df250327c5b"],["/posts/2596601004.html","b21b497d01c5fac6026376e7560320c5"],["/posts/2697614349.html","7c65cc638a9e097d79dd5767c5509245"],["/posts/2742438348.html","26934f437a0826eb1d8176737bcd76a7"],["/posts/2768249503.html","5fca144baaf5f75913f679a78a169eb8"],["/posts/2864584994.html","988523021928b153858fc5fa8ba7c9b6"],["/posts/2888309600.html","d48986e9ca6de8f79245ab4b2987f088"],["/posts/2891591958.html","2818673f91f7082fc6343a62263d8897"],["/posts/2909934084.html","125832f7e0722524fdf7441b5c30c10e"],["/posts/2920256992.html","e2ff950a06bde3e4f577de0144609fc3"],["/posts/2959474469.html","f93d6a860d74e607d91e9fbe119b4291"],["/posts/3005926051.html","d37b6568cc5968daf4bd0805db5fe342"],["/posts/309775400.html","a79f42ced4fed6a796eacbdee1b2c458"],["/posts/3156194925.html","1def96c3ef1fde7077b0f9eed7a9a7f3"],["/posts/3169224211.html","af478b04b8b254aaa5ff4eee19dd8149"],["/posts/3213899550.html","e2a86e701f4db78ae21ba0cf8546e53a"],["/posts/3259212833.html","6a48c36c480f45fd29606e12bac6aad6"],["/posts/3265658309.html","49c91c704f3f3d3f20f6210bac5ee647"],["/posts/3266130344.html","df90fa5b0ac4c3682131c7b7dfaabc8c"],["/posts/3292663995.html","e6c57cfac57b9e25ae79b58e8edc2c48"],["/posts/3297135020.html","71d63754f2001a3cae8ae87b84c8ffd4"],["/posts/3306641566.html","fcafebe7a0b4c3db4290feec412d980d"],["/posts/3312011324.html","c4cd5fc6f72af36e2afcb66040692565"],["/posts/336911618.html","ea4183017fa72b9165faefae887ec8b5"],["/posts/3402121571.html","9f3433e2f819762ce1f53bc7eecd1d62"],["/posts/3405577485.html","5713b96fb153b178d8bcd9581a8a21e7"],["/posts/3498516849.html","88c2f6e5181adf867b7b61b9c6c9fa74"],["/posts/350679531.html","06592006358783bc126a64e5336a00d2"],["/posts/3513711414.html","8aa427d1357c282c8161820fef5cd5c2"],["/posts/3523095624.html","84f0bdc2a63e158f0eb1755003b4c7e4"],["/posts/3546711884.html","e7d154be3aeb34fc80675522a45086ee"],["/posts/362397694.html","a6ab3409de48c05edc26e925464a52e3"],["/posts/3731385230.html","f78d32f56a11f3764d662bc98642b4c6"],["/posts/3772089482.html","0b0ab342a0ab0e7523605512a5b648c7"],["/posts/386609427.html","1b279e1e284fc32eccd43d2776f6859a"],["/posts/4044235327.html","0d63212cad3213e0028bbcc0bce4a238"],["/posts/4115971639.html","dcfe894c58465ab07c5ae548b0097814"],["/posts/4130790367.html","f8e54f88c16df87b4e26cca350b003d0"],["/posts/4131986683.html","3ea8500ef41ec4216a0e6fcbd407d3fa"],["/posts/4177218757.html","236f630f08622e06817b1290e8d06c73"],["/posts/4192183953.html","7e1513ceec94201db942ddfd9d3cc3b8"],["/posts/4223662913.html","c2e686b3b19a00b04ae6b45746105c0d"],["/posts/4261103898.html","0b841a3496f9dd64c3dd8adb8cfad648"],["/posts/4286605504.html","593ff5ebd225834d57d0bad43f887f23"],["/posts/449089913.html","c323822283ec67b207b300024caaf693"],["/posts/469711973.html","1a146ea12d470e0b47c1808151aee313"],["/posts/482495853.html","9e5a5ba6c3c7c218fd568d9b509c2963"],["/posts/488247922.html","81b31334bbfd1fa57ed6b58465113f67"],["/posts/517302816.html","254ddec9517cd41cd95db9f0c4e790dd"],["/posts/570165348.html","3e00c326f5e9db33f863248ec3d2f036"],["/posts/595890772.html","cb905ced90616f28205bbe9281373a0c"],["/posts/67485572.html","f33a9aa4a276e78c30b1aaf0b971b479"],["/posts/694347442.html","2dab496f7c36ca5e059aaa0c68496768"],["/posts/707384687.html","b7241cf6e48ab6794d62acd72dd755a2"],["/posts/71180092.html","ae2e81a9025ec0385c435562b303c00a"],["/posts/716459272.html","94d9e71871b60abc98e25986464911f0"],["/posts/765481613.html","39ca65702b56d1845643f64cc29b5a6a"],["/posts/778231993.html","e562df349305fd9ac0cba3e82579842f"],["/posts/795397410.html","930fd3ea61d447063727f0430b78e690"],["/posts/820223701.html","26a2ac9ef8d042a1a8a4c0385555124a"],["/posts/830372185.html","47dbf0afb62bf0f8c32b2e11d8f985ee"],["/posts/88294277.html","a8e17a48993be3057581c300dc149c3b"],["/posts/939963535.html","894c9b9ee59090357a6c5cf42c01b8ac"],["/posts/983786067.html","d09ccc6b218a8d1cb037b06769fe531a"],["/sw-register.js","f36f6e3bec903f633bc37e3604fd39da"],["/tags/C/index.html","b5539d4438731dac8f5cd16412146997"],["/tags/C/page/2/index.html","7e5193bd9019e898f55be3a4c35d93cb"],["/tags/C/page/3/index.html","fc0965edcad2b4079a378ba6e6832716"],["/tags/C/page/4/index.html","d3220cfeaae0620a20f79c2f6a73293c"],["/tags/ETL/index.html","55107bd2bfdfa393b98323e187d216c2"],["/tags/ElasticSearch/index.html","97b354c15d64ff81ca39850ded757963"],["/tags/GUI/index.html","f818e46cbd14cae314b87c719e8f24bf"],["/tags/HBase/index.html","fdfaf1b4c8bdeb6c48e61e86b7ac6d91"],["/tags/Hadoop/index.html","42be94f11dfaf7355c9b223d5efbc522"],["/tags/Hadoop/page/2/index.html","86950f48f252ca0fd7ad8b2945a36069"],["/tags/Java/index.html","7df63a0f666ec8c90f6ceb6f8e948b76"],["/tags/Java后端/index.html","c3212a0ae090f12bd9bb8a6c02961a74"],["/tags/Java后端/page/2/index.html","ccbb19fa9cfb3282d0f40643aac20060"],["/tags/Java基础/index.html","a4476735d97284371c2346027b74f50b"],["/tags/Java基础/page/2/index.html","4621a6bcdef5569e62fa72ef25fec10f"],["/tags/Kettle/index.html","f81ef9871ea5482f049ec25523ad069a"],["/tags/Kibana/index.html","4b4fd1fe411a79826e756fd6ded92796"],["/tags/Linux/index.html","23ad4e6ecc91be5df4af7185f91206a1"],["/tags/Linux/page/2/index.html","3c0fb9ddf35ca2a593de507de3c78b02"],["/tags/Linux/page/3/index.html","80af8c8b15689a7017cfcbf24d1a3e32"],["/tags/Mac/index.html","b098fac2ed0796e09530de8349b70545"],["/tags/Mac/page/2/index.html","677fe1ea138f2e2dc1d6db03259a14ee"],["/tags/Maven/index.html","92b652ef354f673ff67f70ebd09ffbdb"],["/tags/MySQL/index.html","529209acafcb353d865fb4a9a0881676"],["/tags/Python/index.html","b016eb247ca341d0f5a8713426139d81"],["/tags/Redis/index.html","971a0f94a9d257aa4abeaaf674acb4ae"],["/tags/R语言/index.html","9eaf38df9becc7a4620472f881b65bce"],["/tags/Spark/index.html","6342e45a2bd70c065212f9a08e620ed9"],["/tags/Ubuntu/index.html","9b0c443acdc3e05c938837055a426602"],["/tags/Vue/index.html","b626bc39d6f82f3c74a7ba3f7e815b17"],["/tags/Windows/index.html","025526b4d1117c0f93e46e3c3baca5bf"],["/tags/ZooKeeper/index.html","a3b9d790d00acc803308b9eab1d54c20"],["/tags/bfs/index.html","c2c8693af838c6e4d3c33db287b607c5"],["/tags/dfs/index.html","335b9862f667032b6843e17ba04b84aa"],["/tags/folium/index.html","f1cbfe20cd09d625aca5bcfff9f9af16"],["/tags/git/index.html","3bdd7084a9f71d5458882ec8ef8b0756"],["/tags/iPad找电子书/index.html","1891c37ef14b074cfaac66e1503a91bc"],["/tags/index.html","da8dbc5343c589c56ab18fa549beba0d"],["/tags/latex/index.html","9a55b7f252ab971cf55e3817661beef5"],["/tags/中间件/index.html","1a041b743b522da64e0e6b03a36353d6"],["/tags/二分查找/index.html","bbf562bc0f5eb8c580376b61978c231b"],["/tags/优化类/index.html","0e01225ce3c8b10a28362decf50e883b"],["/tags/前端/index.html","2cc847bbbd4de2e00eca8fa58d42ae73"],["/tags/前缀和与差分/index.html","765eac15073426ec454163d836bd7075"],["/tags/动态规划/index.html","eed78401868bdbf8ae1726d5c014cf19"],["/tags/动态规划/page/2/index.html","8f63c2a8beb6ca87c4b1a873bd06513d"],["/tags/博客搭建/index.html","fa02affa68a6413de43e102e9678f5c0"],["/tags/图论/index.html","386e5f3d1ce07be7474d424a120d0982"],["/tags/大数据/index.html","f3728c2a5b465f92bebb791ec8329deb"],["/tags/大数据/page/2/index.html","81e75e4e4108eb06ed17226d7f4657b7"],["/tags/排序/index.html","ad95e8ac600f5f6fc1765b450d418ceb"],["/tags/操作系统/index.html","32706ff5e7c2f019a777682e3ea25d43"],["/tags/数学建模/index.html","5c4543a1aec2ffeaeeaf411b4ad09ab0"],["/tags/数据库/index.html","b4ce91fb740e2bc32c49bb8c89f0e40f"],["/tags/数据结构和算法/index.html","0b1b8c71c18087cf4ee977a0998d9c67"],["/tags/数据结构和算法/page/2/index.html","17a6bfec9e33381fe6fddd9453fb57f2"],["/tags/数据结构和算法/page/3/index.html","bc95950c524d2ea328020db44eab8b7f"],["/tags/数据结构和算法/page/4/index.html","ae0dfe0ca73b2ea95480957fd43d6cb1"],["/tags/数据结构和算法/page/5/index.html","f52c4642a8b7271ea297638c117f7bc9"],["/tags/数组和字符串/index.html","f75c114cbba3558b4e7074adc83605bf"],["/tags/数论/index.html","68a3d6f88ed6907f171c4114dbff588a"],["/tags/枚举类/index.html","863303c10404cb41e511bec6ccf81784"],["/tags/栈和队列/index.html","85f23604e6192be9c12e5c1a21999cfa"],["/tags/树论/index.html","f441dd9519bee7102128be096f147ed0"],["/tags/测试/index.html","c53f7d1ce8c86ddf9fa729a0894ba26a"],["/tags/环境/index.html","08f1476dbb11d52962594ead351a7dd0"],["/tags/环境变量/index.html","4733a0ac569d558dfb092d4e77c16433"],["/tags/绘图/index.html","61163dcfcee90c46e2a4d66cfb6dcb0b"],["/tags/编程工具/index.html","9baa3b4e63506ddbd90c260b3e8fa8b0"],["/tags/编程环境/index.html","9817caff00d7781ee8f995043878b686"],["/tags/网络编程/index.html","c38efe112eda67785003de8c5415d062"],["/tags/英语语法/index.html","50339a347eda3a49e9ffd64c24d4cd2a"],["/tags/计算机操作系统/index.html","65a9c78e1980c7db352d8fe5c1dc7f88"],["/tags/论文/index.html","7470786997b2a47aaec731e664882e5f"],["/tags/资源下载/index.html","a35ca7d166be1c6ed379f024368a27f1"],["/tags/链表/index.html","8ab417d696208ecbb103707b1ec4b4df"],["/tags/集合/index.html","1e157f95c5d8c57afecf843c715fd01f"],["/tags/集群/index.html","e280d3c6abf9b86f81c5d0c7326b3bc4"]];
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
