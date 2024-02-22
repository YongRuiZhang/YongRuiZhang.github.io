/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","4b80b11f7e4cc3d8570a92389c87d267"],["/about/index.html","00b5ccdd73eb140c09e49593a1976950"],["/archives/2023/01/index.html","9ab957475608bd63f1fde7160f1d0081"],["/archives/2023/02/index.html","8bfbecfe7d4429c49a2fc8fe4be08fde"],["/archives/2023/02/page/2/index.html","ecb6fcf8f43b620f54c79094ece9cafd"],["/archives/2023/03/index.html","b400ea843645c4ccca722ab5d49ab61c"],["/archives/2023/05/index.html","4f0e5d92a29435562e2dec8c0f164c49"],["/archives/2023/06/index.html","b558a23f19e5064c6571ddccb884ae22"],["/archives/2023/09/index.html","430ff79b30f99d90e707e1d57fbbfa7c"],["/archives/2023/11/index.html","ed87b8ae21a74a8c04dde004489cffca"],["/archives/2023/12/index.html","81318cccdc7ed73fcb336a8d03744019"],["/archives/2023/index.html","5f938ce098380576cd9a1afbc17a07ef"],["/archives/2023/page/2/index.html","f20fcb2d7074a2a737977a2995bf4b30"],["/archives/2023/page/3/index.html","3109fce6f00f3780e09d6552fda2993c"],["/archives/2023/page/4/index.html","fc0c513705f61baf96551ed584613b76"],["/archives/2024/02/index.html","64214888b5f4cb2b748f56e042da2f97"],["/archives/2024/index.html","e99b2783c6ddb475b20c436047196293"],["/archives/index.html","5aee8de737a39216d2fab4601f616ca1"],["/archives/page/2/index.html","78e500a2e82a2932baf9a2151e8ad6ef"],["/archives/page/3/index.html","6f3a4661f2ecb75df5be424e75ed7c3b"],["/archives/page/4/index.html","7aad39a68d2ad70ec4ab8715c2830c7d"],["/baidu_verify_codeva-qQP2iZOMLX.html","271a61919828c4edd62f5277f39dce90"],["/categories/Java/index.html","6f078d071ae25c2382eed68e42b1b104"],["/categories/Java/后端/index.html","334b0dd1388c4ca7bc2b4f9bfd3dd250"],["/categories/Java/基础/index.html","d619f65e2557a76434707a106f0809af"],["/categories/Java/基础/集合/index.html","52bf90a6cd89f169f050cd07d73ca17c"],["/categories/Python/index.html","6e3baed97b1973c041a6e25a0b8a2b76"],["/categories/Python/编程环境/index.html","f6b65cd3287c0a98e59190ac2bafdab8"],["/categories/R语言/index.html","ba2e5e2be9e45b5842338fb046e44224"],["/categories/R语言/编程环境/index.html","deef1a15c4d138dda544e90c472ba402"],["/categories/index.html","5212b72743bc736f41252d4bc7d513b9"],["/categories/中间件/index.html","e852045118fae9eba55ef41662cf0a01"],["/categories/前端/Vue/index.html","da1e8537201d03999be35ebe0ab3d9d5"],["/categories/前端/index.html","a32b993cf0dec9c4a9a58bf5f4fc1ee1"],["/categories/大数据开发/ElasticSearch/index.html","9cb7ae1f8b73cbfde6e7f340c43b525e"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","056a4ee6d9a0c273cc51b9d5142b445c"],["/categories/大数据开发/HBase/index.html","884f9ff22e537055d8d23354dbb3bb1f"],["/categories/大数据开发/HBase/学习笔记/index.html","b16905d51f67f8fea3b5a1f0a4f9d056"],["/categories/大数据开发/HBase/环境搭建/index.html","3e878bb4bf922be1ee64de026141a78c"],["/categories/大数据开发/Hadoop/index.html","2cc407414329ed7dd2b6cfaf0b025fe7"],["/categories/大数据开发/Hadoop/技术/index.html","9837902988fa69a457f9c282d190669d"],["/categories/大数据开发/Hadoop/环境搭建/index.html","1b1fc2f66ca37a35cc1912ea0e3e992f"],["/categories/大数据开发/Redis/index.html","e11b59754ff62fec41f650d621f6bbce"],["/categories/大数据开发/Redis/技术/index.html","aed4e31593eecdeb85cf0b6f61bd0b75"],["/categories/大数据开发/Redis/环境搭建/index.html","30e1fb12a7a5c10ee8595b6362af81c9"],["/categories/大数据开发/Spark/index.html","5cb2d4c99201b1126ceb395cdb7c3854"],["/categories/大数据开发/Spark/环境搭建/index.html","350c1f435b1b3f0320f7a413b33c2532"],["/categories/大数据开发/Zookeeper/index.html","d25465c321e85a01b36d22768f0bdc19"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","c7373cac8b975b5d45a915ecb84c3d51"],["/categories/大数据开发/index.html","ec82f4ff4568a6ac9c6d969e128e2e0f"],["/categories/学校课程/index.html","536f2e8cf55e6c4076ae11b305df90fd"],["/categories/学校课程/计算机操作系统/index.html","87b28022599af8348aa123dfee634057"],["/categories/操作系统/Linux/index.html","4829788ff78a2d24ca7c44562272f617"],["/categories/操作系统/Mac/index.html","0fd8643b26de5a7b0161f7ac3663c28c"],["/categories/操作系统/Windows/index.html","d7f4ccf9e500c87b7879289409ad0d2b"],["/categories/操作系统/index.html","d60d659a70554707e3b9a489e20ef63b"],["/categories/数学建模/index.html","54a459794fe0b4fa303a3bf0a71e982a"],["/categories/数学建模/latex/index.html","183a7ce7f3a53d3ce428391274b3d05c"],["/categories/数学建模/优化类/index.html","25db3c0834db77afb31737d757d17a8e"],["/categories/数学建模/优化类/现代优化算法/index.html","4d1da735f14ea789d52d0eda0f33fd65"],["/categories/数学建模/优化类/规划类/index.html","62643a6b77bfca73c5c2737b94921e49"],["/categories/数学建模/绘图/index.html","b3741f4fcf85d2536e955fccfaa08e13"],["/categories/数据库/MySQL/index.html","6af38cf5ab5ac3a47ba3301afb8eaea8"],["/categories/数据库/index.html","6e0338e1fa049dec9d9812fd7905b4b0"],["/categories/数据结构和算法/index.html","52a4aa347fbe61b2cb0824ef1f7f1ae5"],["/categories/数据结构和算法/page/2/index.html","e8b073d31fd0b80d65ced5798adf372f"],["/categories/数据结构和算法/基本原理/bfs/index.html","15959c76735c73f8995e683c11401921"],["/categories/数据结构和算法/基本原理/dfs/index.html","31d0a39b2ece21a3f9dd718fa77eab35"],["/categories/数据结构和算法/基本原理/index.html","02b884af22331e3e5e6f5b00cd008cba"],["/categories/数据结构和算法/基本原理/动态规划/index.html","50841aa46d590ea0fdae4f0526446d0c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","4dcb2714e0bac3deaa19ac28f70641dd"],["/categories/数据结构和算法/基本原理/图论/index.html","0ae6876316c859f2e039f75aa60a0856"],["/categories/数据结构和算法/基本原理/字符串/index.html","482a54bca52f819b8fa6116d4e79c281"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","e04575d1ae3b8266ef3b348ceda58eb1"],["/categories/数据结构和算法/基本原理/数论/index.html","89f93b4d0d9d1275a223336a68f2d238"],["/categories/数据结构和算法/基本原理/树论/index.html","14344a5f971e87e4094472f0adfb0b6b"],["/categories/数据结构和算法/基本原理/链表/index.html","3e3a740a914761675adc6d336548bd66"],["/categories/数据结构和算法/算法题/index.html","c81abb145caa90e471c0d7b746f32589"],["/categories/数据结构和算法/算法题/二分查找/index.html","e4a69b8e62dd447575db708a8836b993"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","724c92cc721442592ee6252ebe6910fb"],["/categories/数据结构和算法/算法题/动态规划/index.html","04df2a47a8ea779056260f5fa54e2126"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","423ab90ea729f7160374d5ec799b6ed3"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","04e767b796942d75718359c55358fcca"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","bba18cdbcea9b32667864a0fd2c7be75"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","03cc37cdb4db724017cdae2639d64e0b"],["/categories/数据结构和算法/算法题/数论/index.html","a8e0cf83716aade9c99e07603453af39"],["/categories/数据结构和算法/算法题/栈和队列/index.html","88e3e3723acc0c625ffee1d4c1cbae8b"],["/categories/数据结构和算法/算法题/树论/index.html","f191d4b5703cd0a2648969132d52bd88"],["/categories/杂七杂八/index.html","a50a72882db28a52d7144d572cd180e7"],["/categories/杂七杂八/博客搭建/index.html","9a418dc6090b1441c7ccfa607dce63d2"],["/categories/编程工具下载/index.html","7a9d9946074a824b977794dfc7b841ab"],["/categories/编程环境/index.html","1dc3ddd4ed653202b5663fb34fa5e613"],["/categories/编程环境/大数据/index.html","f37d6bf237dc08d8f717856ded085312"],["/categories/英语学习/index.html","dfdeb5ad919d8d5b5f6eec97122de5a2"],["/categories/英语学习/英语语法/index.html","fa21ae656a819f504e2007af506ce746"],["/comments/index.html","5f838748bca960693ea6c275f9914c31"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","d7e839eba8035c5b3bfec51f105380ce"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","c9fc55d89b583225212ce1149bb47a7a"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","1b13b741ea803c0ab37027e401d81bfd"],["/movies/index.html","ef8f98cf3602ea470f2e774ddbab3740"],["/music/index.html","d55cba9ee610aac9e1191b2385ec8d46"],["/page/2/index.html","b4b691c252c11b77935eb1ca5f6a4acf"],["/page/3/index.html","1627917664956def9b3d5dabfa210405"],["/page/4/index.html","53d5d22f64d29cd1bd94cebd9a03309f"],["/page/5/index.html","e7f2ac647d918c93e0bf7d3af5ab0dcb"],["/page/6/index.html","54449b18f54d05c9cd2e337580e185f3"],["/posts/1021360842.html","7ad04250635cf21bd619f3a9d70edb50"],["/posts/1120620192.html","bf2ed56de9e56bf2fe404ecb4019f798"],["/posts/1141628095.html","f3a0d8bb6d4e0b1e56dd029a8e57638c"],["/posts/1168613674.html","89025626564fe89b42ab00fefafa43ca"],["/posts/1219920510.html","b04722f804aebc85dba46fd6e4dcb3e2"],["/posts/1222166338.html","b69d4edd9843c6546b243f6d5216348f"],["/posts/1259097482.html","892f3a27e8c5faaf572bffd2d0513e71"],["/posts/1271036369.html","3bd5f11d3de2b6ce4c73a62b2cb43af9"],["/posts/1312847445.html","27d0b4f2f45f26dc5acecb678cccfeed"],["/posts/135355774.html","195825ac76cae44b4097e93ed73ba625"],["/posts/1375344716.html","6e060437c8cb9c32c573aaf44248316a"],["/posts/1388991698.html","2f8905e3e3ca720ed6df5ba7c4b87967"],["/posts/1410315814.html","765f39643fbf10ac6223de4e4b0ddb85"],["/posts/1452790229.html","20ecf1607abf403a784eb3ddafdfb304"],["/posts/1470079884.html","1fd0d141e558084e26425cde246ae978"],["/posts/1470079885.html","24d15ad146072782daa8ec6daf439f9b"],["/posts/1470079886.html","fdd5e1bec68043e67f80e0c89abc4c5d"],["/posts/1470079887.html","8c8f729fef7a3166c2f6c3464f7a8fca"],["/posts/1498536549.html","47870b818f23411117145fc662f85c11"],["/posts/1539568593.html","f2ee1f0bd4673907c1a7ec627f7a8961"],["/posts/1547067935.html","e38339f53128ad862ec69ef1842a708f"],["/posts/1557866301.html","5622c2aab47ac483ade8a927126927b0"],["/posts/1571776361.html","a5e225959b315aff66908d5cef20cf61"],["/posts/1605124548.html","aef45346ed8ccbe1077b6bb77515b0de"],["/posts/1633036852.html","b51ad573eb103c1f0b27950c783d12e4"],["/posts/1674202625.html","6925b48d920b5824a458357e0e54c279"],["/posts/1765123828.html","b655718d58bb3b565e4a8aa85b3d8dfe"],["/posts/1767336200.html","fdd6836b55944fb774d49526bc633cc5"],["/posts/1776114197.html","22bb53cb006a4ec9d2ff21d531019cef"],["/posts/1817748743.html","8fbf8fe70e2f79450e517eb463b10af2"],["/posts/1925125395.html","21802a156fcb4e767a0a9d17bbac64d1"],["/posts/1966191251.html","72283a2ec929941a80647ee34db87043"],["/posts/1987617322.html","d596267042b1669334928f0b22468b68"],["/posts/1999788039.html","2e83d8d4ebd6c411a318044b272d5f23"],["/posts/2075104059.html","42a2ea256e744feee2414d16e22b0261"],["/posts/2087796737.html","6d8653905f08283c3006721737233dfd"],["/posts/2106547339.html","fafd86e33a89080842e7e8cfbba2dda7"],["/posts/2207806286.html","4776f59317dd6cfebac1125efff029c2"],["/posts/2225903441.html","88f5f4a9e837db36b61602df95ed8a43"],["/posts/2265610284.html","f0ca42a4a1ceaab5ceb3182be03be3ad"],["/posts/2281352001.html","b5a42b57c513accf343c015096a6ac6d"],["/posts/2364755265.html","c1f225e44be656596aa9f9812cc9917d"],["/posts/2414116852.html","dfc83eb83c48bdea78125d084c947c02"],["/posts/2421785022.html","0b2f9eaecee78bbb852e2c5e3d4bb3bc"],["/posts/2482902029.html","cf606cbaab68075996af717b88ce2619"],["/posts/2495386210.html","07d19f704fbb656c5b73596b80e9e0b9"],["/posts/2516528882.html","3b0e6de4cf22ff61e5f419e608714cee"],["/posts/2526659543.html","b746e10518d52319597804ee3fe0264f"],["/posts/2529807823.html","0fd53cb2207c769fa5fac32068cd35ba"],["/posts/2596601004.html","208e50d5111955df7064e323bb034920"],["/posts/2697614349.html","a6ab73b5cbce90f91698595617878c58"],["/posts/2742438348.html","d721fa80cc1d62bfe35b26115eb9d0e4"],["/posts/2768249503.html","1d47ae43a11b43ab038d10193d554990"],["/posts/2864584994.html","dd258844d60d7547ad93c2c3718b86fc"],["/posts/2888309600.html","944416f745943230aceab463aa73690e"],["/posts/2891591958.html","835949688dfee9e8b444ada5699214e5"],["/posts/2909934084.html","70054f926e3bed1f3a0c0501e6c8487f"],["/posts/2920256992.html","68171ff3346579e1eafccb43eea7f74c"],["/posts/2959474469.html","2db4d14bdc4f45e83333ba9e6073375f"],["/posts/3005926051.html","e89eeac56efbd37d4c3018d163a2cab4"],["/posts/309775400.html","e7051cec255acff3a2adcf376b179997"],["/posts/3156194925.html","34f19aeb297891c8f096df4ff5025c93"],["/posts/3169224211.html","607ae99c9744898ef2503a0f3ad2824e"],["/posts/3213899550.html","0e7635d91427f70c0b184967807a1753"],["/posts/3259212833.html","c06f8d8865d21978c5ed96ee5d83f7e6"],["/posts/3266130344.html","6566baf50d55ae3ca857645e892bcf5a"],["/posts/3292663995.html","c03edc363318dbc782af973912aad55d"],["/posts/3297135020.html","a9ad9016a35320c64ee8b629eb56956d"],["/posts/3306641566.html","396d4d667a2cbd2aa1b41d8b763b25ec"],["/posts/3312011324.html","22edf37c5a859eba6dfe4a3ab61825ea"],["/posts/336911618.html","be052b1685036b24946f7a591a0c2638"],["/posts/3402121571.html","6ff6ca33d3c56a2312ad3f81c822aaf4"],["/posts/3405577485.html","637820111d2ff0c2330e3583abe6cd43"],["/posts/3498516849.html","f274b1cfa25b456191470d35861a7bcf"],["/posts/3513711414.html","88b3364af7b65ed1c024d1b00e03daca"],["/posts/3523095624.html","4e95528845311c68f4a8893fce5cfce5"],["/posts/3546711884.html","c4cca5eca3022db1ba78f789abd6a7e4"],["/posts/3731385230.html","f8f3ff549e0d19f95742b5c0791008d4"],["/posts/3772089482.html","b76184f6ef58c692fc8ad8688721fdd3"],["/posts/386609427.html","90cccd1e9c0ef619cfe8537b795a8818"],["/posts/4044235327.html","e66fbbbc93941d8fc6229d2b9f556fb0"],["/posts/4115971639.html","e966cb13b6f03dfb7ab555b524a1805e"],["/posts/4130790367.html","94291456f8a3e534f49b436269a153bd"],["/posts/4131986683.html","08b69c093959ac80920520e589b0a029"],["/posts/4177218757.html","ed3a25f3ab27d715e3a6cd6e17e10ee3"],["/posts/4192183953.html","cf546b4a7b1f3cdfef2ac4dde404c2bf"],["/posts/4261103898.html","a3287a98de7817cd5af9f1c9300e45a8"],["/posts/469711973.html","1245a496f3b63273a19172ea6646c1aa"],["/posts/482495853.html","ebdafdc45b9400a0e0ed893b847b54cc"],["/posts/488247922.html","e83dd7919a5f33eb232562ea9ace2b26"],["/posts/517302816.html","0b6659e51e62ad946a8655b283065038"],["/posts/570165348.html","5ef1fe019ae28bf54efea4df8a2b7605"],["/posts/595890772.html","1a0750c561484f421ff2807de3f56793"],["/posts/67485572.html","36d4c8ab8a7ccd57b32f66b7ad923a95"],["/posts/694347442.html","6781321a813ce20bfcf96473494cf031"],["/posts/707384687.html","c4d856a8abe0e57aadf45a791b972b60"],["/posts/71180092.html","fda38d0e66a5c0e20690682133e0a1c6"],["/posts/716459272.html","f4439db798f7cd1e19e26bbaa3382bd3"],["/posts/765481613.html","ef49f8044e2a759c019c7c23a6e91fb6"],["/posts/778231993.html","68869b16a7980c0d4c6bd543b846c5a6"],["/posts/795397410.html","2c6c743db8339cd8ca8d921c226ccc1d"],["/posts/820223701.html","200cc066f08a17081b01263d3834008a"],["/posts/830372185.html","fd3e5128e076c1150b8952dd6ffc1b01"],["/posts/88294277.html","cab7d12e2cd7f34fc727278e24f41fb3"],["/posts/939963535.html","5f6e95e780b73f2ad527afcbeff70da9"],["/posts/983786067.html","aaf905092a1c62d20219869981e345aa"],["/sw-register.js","c7632f5d33eb49cc8ee6fe2c530b14d4"],["/tags/C/index.html","3b85767a3cea4f31822aca8efd2b75ae"],["/tags/C/page/2/index.html","1e74279c91299057749380f82fe9cf0e"],["/tags/C/page/3/index.html","158393d5ade4aa95f82835b9ac9a23b8"],["/tags/C/page/4/index.html","a11baac3aa112b5fb1eabff98e038ab1"],["/tags/ETL/index.html","1799dd254941dfa523893f12b2aa0440"],["/tags/ElasticSearch/index.html","b6de8ab8d059a7ead577d58690a975f7"],["/tags/GUI/index.html","0734a990545d90f4f103a853c2121ad4"],["/tags/HBase/index.html","5a1e19de43668d95d8ec7d5b5b52c74c"],["/tags/Hadoop/index.html","a98381bd01a25df2aaa38dd4de3e27eb"],["/tags/Hadoop/page/2/index.html","932989335da4b44263b58d92d5cb2396"],["/tags/Java/index.html","c76e39f78af4d4043ee4310b63d15509"],["/tags/Java后端/index.html","2cc65156d50b1f1f6134d809ce3184dd"],["/tags/Java后端/page/2/index.html","132333361be922b0f7fbccfe9dfed8e3"],["/tags/Java基础/index.html","87532fcb23f3fc324a1cb51d70654518"],["/tags/Java基础/page/2/index.html","b9dd4d779aba71fb4663933460f3f8a5"],["/tags/Kettle/index.html","331ad9a9cfa9ec548dfa13f4b86e7cd8"],["/tags/Kibana/index.html","90ee19b3748d35b53704fe4820c8d507"],["/tags/Linux/index.html","2ec3e2de58bf71d7a4adc9f84769f864"],["/tags/Linux/page/2/index.html","ed0392f18d2013772ed449448ca8d919"],["/tags/Linux/page/3/index.html","57b80c17b26aa716b66d9367735b05a0"],["/tags/Mac/index.html","a7842759b13b14254ecae6175df98bb4"],["/tags/Mac/page/2/index.html","179a580ba2cd3e1fbde8650ef1d3b24d"],["/tags/Maven/index.html","e42fe03856f59b1200cf5ae825244882"],["/tags/MySQL/index.html","7c251078ad4d5390c7da906963eeec23"],["/tags/Python/index.html","1d9ab4d312df7f7d385eb1d43a2188c2"],["/tags/Redis/index.html","3f6f7058f7a122edd6b5910e5e8fa9a3"],["/tags/R语言/index.html","ccc9dcd6d463dbc4dbb1511642d0e563"],["/tags/Spark/index.html","9353275f8b310ece323dfe7aa80868c7"],["/tags/Ubuntu/index.html","25facddc6c72ceb3b4e9639ac52611cb"],["/tags/Vue/index.html","2b05363ee03213c47fb0d8b4532dffcc"],["/tags/Windows/index.html","5a268e72de7e2fd03f0df6e531ef2521"],["/tags/ZooKeeper/index.html","5f15f0d02a7ed966ec01d2ef18095b5c"],["/tags/bfs/index.html","5ea97fca54e5447766968431483cc9a6"],["/tags/dfs/index.html","d3c18ba9ce861c993d7625722ee4f976"],["/tags/folium/index.html","07916e56944bbd1e3e165aeec6472ed8"],["/tags/git/index.html","67a0c37787b0123a015e5b0b89846bf6"],["/tags/index.html","42bed222590d68ed3cf8905e4f1b7c4c"],["/tags/latex/index.html","ba6ffec87fe2901edb5b056c687dd9de"],["/tags/中间件/index.html","397d32f0cab512cf3a2c57015fa2ef11"],["/tags/二分查找/index.html","f71570e66de4c0b18bda45ec72340b0b"],["/tags/优化类/index.html","8a5b88672c5be9dd7c7ecbd352c10492"],["/tags/前端/index.html","adc92a78ff5e8728f51592dfe7e2286b"],["/tags/前缀和与差分/index.html","05e05008788cedd24c1564e24d23f1ce"],["/tags/动态规划/index.html","1605a92d9cd50c0127f485248166fb37"],["/tags/动态规划/page/2/index.html","84a41bf2b25165ed72d94ff68ace0fea"],["/tags/博客搭建/index.html","9e3af6e4278e11fc3ed2b9c32d1d0de3"],["/tags/图论/index.html","38edef908fdf4ff3aeb93d454c899dc1"],["/tags/大数据/index.html","11b3b5acc4de157fb1524b613e03022d"],["/tags/大数据/page/2/index.html","98798111dd8d83715e8f81fbda0731b2"],["/tags/操作系统/index.html","e722f6a288e995e06444adb19914c0cb"],["/tags/数学建模/index.html","c37b2d5abbc2da3edf08209a7b025640"],["/tags/数据库/index.html","b5dccd54acd6d296d093fd7458903d88"],["/tags/数据结构和算法/index.html","c87b259be68fa4b210a581358c65e075"],["/tags/数据结构和算法/page/2/index.html","d1f5a6f62a08dd4533a284e3ebd83853"],["/tags/数据结构和算法/page/3/index.html","ae0cdcaaecfb556be7f38d18142155e2"],["/tags/数据结构和算法/page/4/index.html","c25d2f36770105fef2a5fb8db7c6ae5a"],["/tags/数组和字符串/index.html","2b0b0d4b7a02e9b6199bdb7e31078265"],["/tags/数论/index.html","f43387922e64efe354cf6a369ed6a304"],["/tags/枚举类/index.html","9067c7c809c7a0059272dbe9e046fff0"],["/tags/栈和队列/index.html","4b321f9fe97313a9fe30239b0e4e47c1"],["/tags/树论/index.html","d92da05b20cb38d52761ce7c765bb8a7"],["/tags/测试/index.html","0d768fa273f43180f74699cdf158ae01"],["/tags/环境/index.html","86e5d3f4931ac4745b67150e0176546c"],["/tags/环境变量/index.html","95ad2d7d5dae3f492d4f6d8b87db0dbd"],["/tags/绘图/index.html","19b681e235ab7d833edd77ba0fd789ca"],["/tags/编程工具/index.html","efa94f973f909248870c64aa9ae82bcf"],["/tags/编程环境/index.html","5de4152667a7477686c63d8b3a186f1a"],["/tags/网络编程/index.html","910da525b553bfe8de5ed11fae763530"],["/tags/英语语法/index.html","6aaf706c39933be6df76a3bed165ebbc"],["/tags/计算机操作系统/index.html","ed8073f83d89a5adf71087b71a0a94cd"],["/tags/论文/index.html","a9bbce22ade3bf558a61628f5331e95b"],["/tags/资源下载/index.html","2d29ef3cdd2e731977faa4196704f479"],["/tags/链表/index.html","41be8b296cf2104f10f366c54c765243"],["/tags/集合/index.html","f6981dae208571642a2b1d6b716c60ba"],["/tags/集群/index.html","0142fa38272b7d2ca225ffd7962250c5"]];
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
