/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","fd6089dda68a79c83cb4f5a70e536574"],["/about/index.html","5cc07c1790c21e8aaa4aacb14ec0482e"],["/archives/2023/01/index.html","e60e9592bc458e67198a0239e61c94ff"],["/archives/2023/02/index.html","33dc963545b2bda30d099acdf2bea7d3"],["/archives/2023/02/page/2/index.html","6a9cd48cc4bba57d9519965aa93a2492"],["/archives/2023/03/index.html","a3f86536afed2f4ef6e14579be77b00e"],["/archives/2023/05/index.html","4dd6dd4aae61e38db73ba0b7203fc25d"],["/archives/2023/06/index.html","8bba2a6240e02fe829b8a1d370a0552f"],["/archives/2023/09/index.html","813a5bdcc8dde8d8ec089798dadc60cf"],["/archives/2023/11/index.html","b826b6e6a99e8f806012acb5e7fdf539"],["/archives/2023/12/index.html","9840c42219756c10880e50b95ce228a2"],["/archives/2023/index.html","77481f776c6265818b806e6939041f0a"],["/archives/2023/page/2/index.html","d00d7b11a45bb55b0eadbebb138bc656"],["/archives/2023/page/3/index.html","85ce490ef5c745fef8f61a4cdcb51a54"],["/archives/2023/page/4/index.html","bab575f17640d654216cdc7046faee3a"],["/archives/index.html","d97ae3dd30064fa964ca3fe12435eaa3"],["/archives/page/2/index.html","a6175f3ba655daf14b6cd9aa13c8d32e"],["/archives/page/3/index.html","a362992d8c4d19a6a6377faa0c16d57a"],["/archives/page/4/index.html","64c5278774315f7432d36740753d344e"],["/baidu_verify_codeva-qQP2iZOMLX.html","dec77b1b480371eac5333e0ba3d3dbe2"],["/categories/Java/index.html","bad3cc685f5ba7adf7141fdd9e72fb2a"],["/categories/Java/后端/index.html","43e719e04d3353ae6854b07037e73daa"],["/categories/Java/基础/index.html","98edec29d46507ea70c098f3e696b2de"],["/categories/Java/基础/集合/index.html","e1f2c695dc1b4219f0901c3a36f34c2f"],["/categories/Python/index.html","caefef232a70ece0eac4b6fdc24834ca"],["/categories/Python/编程环境/index.html","d38385a1f5485a5e4e540d093657fa16"],["/categories/R语言/index.html","0593f00ce98b88bfe19065a5d5e59c96"],["/categories/R语言/编程环境/index.html","d93fb097245ff40092d4eaea7ce80ec9"],["/categories/index.html","ca9a1dd26543e37bcf76efaf87cefa8d"],["/categories/中间件/index.html","57c1601370ff1d5279657c533808d67a"],["/categories/前端/Vue/index.html","fae2070dee58dd9e78c0143b5d1cae3b"],["/categories/前端/index.html","614a7e15744540f35710b1186445759f"],["/categories/大数据开发/ElasticSearch/index.html","c42f51245a66fc8338791f4f8b0a1383"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","1e89d2f959190ca688e6d1323229cb97"],["/categories/大数据开发/HBase/index.html","b884f6879d800d403843b2ef00891491"],["/categories/大数据开发/HBase/学习笔记/index.html","fb7e5afb7874aeddebfa79c8eb16a5d5"],["/categories/大数据开发/HBase/环境搭建/index.html","b247354dacf4a1c8879bfba9ad22f46a"],["/categories/大数据开发/Hadoop/index.html","fc2ee1f3072a04d68240c9a632b44c3f"],["/categories/大数据开发/Hadoop/技术/index.html","702c7000c758683a94c4451e86e40de3"],["/categories/大数据开发/Hadoop/环境搭建/index.html","f2de5fc91206723e832afeb02deab0a3"],["/categories/大数据开发/Redis/index.html","295a476ea78d1fb7da27d43ba958a9f2"],["/categories/大数据开发/Redis/技术/index.html","ab7b828289f36149104eb6bb5662cf33"],["/categories/大数据开发/Redis/环境搭建/index.html","5a0b96fbacff2f621e212538f48b5799"],["/categories/大数据开发/Spark/index.html","32e79b36d96a39a6ff31bafc5c4ee259"],["/categories/大数据开发/Spark/环境搭建/index.html","374f643aebd34473b190c76832618625"],["/categories/大数据开发/Zookeeper/index.html","b894e1fde9f0ceafdb63625a6fcb8095"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","e5b9943356be703bd57b52fd6f3ec59b"],["/categories/大数据开发/index.html","919e4cc01ddd6c8834f230e0e7283475"],["/categories/学校课程/index.html","3ff45308c5df1e6a3fbdf2510081be8a"],["/categories/学校课程/计算机操作系统/index.html","443ca2faf490a39904efe8dd96980a78"],["/categories/操作系统/Linux/index.html","440aed0133490e488ca005e77bba5d32"],["/categories/操作系统/Mac/index.html","c709903a2d624351136797745ac61a4a"],["/categories/操作系统/Windows/index.html","274eb71263a783aa4021a3439efe2a3a"],["/categories/操作系统/index.html","8bdd3449e2610a26756ed4e380c25c11"],["/categories/数学建模/index.html","d73f1cdc7e9988103167fb38803fc02e"],["/categories/数学建模/latex/index.html","965450f72e434d853675366a7d4374f4"],["/categories/数学建模/优化类/index.html","9ab5919b649560528ac3c6699e21b8b7"],["/categories/数学建模/优化类/现代优化算法/index.html","27cb360f34677f8319c14ba40d975f97"],["/categories/数学建模/优化类/规划类/index.html","27828b590ba5fe67121112018d0d56b6"],["/categories/数学建模/绘图/index.html","affd4f6ee2195958a1f6b4cc81d17cfd"],["/categories/数据库/MySQL/index.html","3cb4d87d5c1f6f7ead5110343d421134"],["/categories/数据库/index.html","3d053505347edbb2f53288bfcee86c6e"],["/categories/数据结构和算法/index.html","76583bb2f153934204086ce8ac5a09d7"],["/categories/数据结构和算法/page/2/index.html","a40c6c1a3195a2037fc18e79403462c1"],["/categories/数据结构和算法/基本原理/bfs/index.html","9d481dbe477f3a0d7dfef9ae9787673b"],["/categories/数据结构和算法/基本原理/dfs/index.html","a64a55af502c315add21719010612d34"],["/categories/数据结构和算法/基本原理/index.html","aa348b60691a973c9fd9f602de4e4718"],["/categories/数据结构和算法/基本原理/动态规划/index.html","11d2f3132ae7cf992548ab8d12b5e72e"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","8d975b719e58ba37e39e5ede914292d2"],["/categories/数据结构和算法/基本原理/图论/index.html","b1afea5957d788b008446528f8ba8240"],["/categories/数据结构和算法/基本原理/字符串/index.html","10a09cc0bdf52cafa3bbd40fdf00d9fd"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","84e0bfb00331d2166210364df5823631"],["/categories/数据结构和算法/基本原理/数论/index.html","ba07e8347c2dccea4d29bb3402b753b7"],["/categories/数据结构和算法/基本原理/树论/index.html","ff81785d2df729e6354f08eb4a7cb8c8"],["/categories/数据结构和算法/基本原理/链表/index.html","60226edf0d1f761c3e1477047f9cfc7a"],["/categories/数据结构和算法/算法题/index.html","3f1c16337c70160abe369dec18ca1fff"],["/categories/数据结构和算法/算法题/二分查找/index.html","9ffe0857871635e58c911be98b9f2793"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","c00d557aaed5b0bb9dbf3550a9054b24"],["/categories/数据结构和算法/算法题/动态规划/index.html","4f6506f29ff977d5a99e4c41c6896aee"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","58032b0138536ebbd41a34f965cee207"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","1b7b5ca86186779c04714170915205ba"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","90265a6e330d8f9330b24df7a30b52c4"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","fbfe5788f7aa0b8f3f8da5900f329630"],["/categories/数据结构和算法/算法题/栈和队列/index.html","d7dbd5ba6e60b84cafcc7f7ca5b8371c"],["/categories/数据结构和算法/算法题/树论/index.html","3058489ae4b2f38696a226ec9ff20a2e"],["/categories/杂七杂八/index.html","ac7a59c57edf2fcaa1c7696a23f3c708"],["/categories/杂七杂八/博客搭建/index.html","51f38e7ecef5564ded54f89f45d04ce9"],["/categories/编程工具下载/index.html","98021fd3d3e407fe4369081e5bbf10b5"],["/categories/编程环境/index.html","1b88d8aee67c66884d8e1bb095ba9f56"],["/categories/编程环境/大数据/index.html","4e3f3bc2b1258f1404f0e1b5be9d8f7d"],["/categories/英语学习/index.html","de7e69d9ed8d23d5830c0348e2124ece"],["/categories/英语学习/英语语法/index.html","8e4ca4d5912a6b35f7c2ea5c25120fb9"],["/comments/index.html","45defd948bea55bd6134d56433aa0fc5"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","5d7210ff17840a802172075b00392fae"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","3ae84c46e1fe722c23952ae2923f3a1b"],["/movies/index.html","6d428446b1e123d78c7df66f078c6f32"],["/music/index.html","b6d1ccb21e36f236526435f3d86604a2"],["/page/2/index.html","80316d700eb3785f183d58dc2d74c7ba"],["/page/3/index.html","52b0177f222d0ffc34c7a981bb9ce8b6"],["/page/4/index.html","7bf5bdc5d1261dfa78f340991bdfbcd1"],["/page/5/index.html","c7c255a3df928670ebb8806bd0c7bdec"],["/page/6/index.html","32a9921730339ef68601fd1e87811b21"],["/posts/1021360842.html","74c9a8c06afcaa427f7f46b99f8ef295"],["/posts/1120620192.html","078946da5c3194948368f6cecc8aa1e8"],["/posts/1141628095.html","ef7ba9fa8b3748c67aa57733fe5c3fa0"],["/posts/1168613674.html","85d4335d809f95c5a1caa2b9bbc19691"],["/posts/1219920510.html","06ac98af2d209ec0f12717ddc3378485"],["/posts/1222166338.html","428022f7bc4b62a569f67cb1859a1357"],["/posts/1259097482.html","33bca9f430c60326bf07e7c2a040c391"],["/posts/1271036369.html","e75bd0ef479cfa9267cffd369f658f5f"],["/posts/1312847445.html","078b20492fd03f655c9cf65bfb6aabc9"],["/posts/135355774.html","33406ee197ea5e6258c1a945d0e6b270"],["/posts/1375344716.html","39e7e1844208ff59490a24e89089fa46"],["/posts/1388991698.html","d429abb94e01ed427dfd7543fcfe8a95"],["/posts/1410315814.html","cb21a0d9a666ea65088616b71fc031c3"],["/posts/1452790229.html","6180492e3b321e1d8ddc193b3ded8d59"],["/posts/1470079884.html","95328c53b1546f66f1b632697ef12f9b"],["/posts/1470079885.html","2bd79a1daaae18338ae223685c1a3d4a"],["/posts/1470079886.html","376fdd744f59710f51769cfe611c55b2"],["/posts/1470079887.html","6bb0f42b54ab86da12031cee777f625a"],["/posts/1498536549.html","d45b5730e5395d2892169944398989af"],["/posts/1547067935.html","d2dac1aca9654a0cbb0d5939b34b21b7"],["/posts/1557866301.html","4c0aad827a90abbeb57e9e9f7ed9eeae"],["/posts/1571776361.html","058f4cff38c50ec4cc933c86c4f258cd"],["/posts/1605124548.html","c64ef60a5a8f47cc5fbbd44a9775995d"],["/posts/1633036852.html","bf367d126e5ffdb40571bc32287db5b4"],["/posts/1674202625.html","971f5da86959132ca4df67b44c6db5cf"],["/posts/1765123828.html","aa1420d8b104d86aa2653a8679a9488e"],["/posts/1767336200.html","0be74a2ca2a023a3cb2b513859b8993b"],["/posts/1776114197.html","dee6a70cbe6b147eaf3643f682658b40"],["/posts/1817748743.html","90a6e820d0906aea42acf7d9220ea72d"],["/posts/1925125395.html","79615d4d10b7cdedbe4b429e02a47071"],["/posts/1966191251.html","6a72bcaa1168c3f781330d6f8ee7cd53"],["/posts/1987617322.html","ca35fcf1ee2c8269fa8c7a24eba60fa1"],["/posts/1999788039.html","22048e52e20dcfd53cdad6ff098f7d8b"],["/posts/2075104059.html","e644e2651013b291151f416a32f7993d"],["/posts/2087796737.html","cfec84612913a758aa7a642cdf8e515f"],["/posts/2106547339.html","7d113367c80c72f40e7bc4a0c2374e6b"],["/posts/2207806286.html","3c41adbe347ce01f46e1fb19280f448a"],["/posts/2225903441.html","41815f3032df3baa90015fc970b92d2e"],["/posts/2265610284.html","ac65a14ef29a490ada798c0da645286a"],["/posts/2281352001.html","e3cb2cb943a4de34e46406b122d7f725"],["/posts/2364755265.html","62a09442cef51aad03795bb9bf44a7e2"],["/posts/2414116852.html","e9328bf8446cfea1f48a70247ad1c890"],["/posts/2421785022.html","390efba32aab3073a56b02d8b36fb580"],["/posts/2482902029.html","b46b8f32cc78a2486d42da8de3180407"],["/posts/2495386210.html","e177001f54c0ba1d330d48726d894734"],["/posts/2516528882.html","7b1f66df6a59bef50ba3f033cc85a7da"],["/posts/2526659543.html","40a896077ddecd1db19bfdb793c7a7e8"],["/posts/2529807823.html","07234d4dda7fc4fb29db49e9adce7774"],["/posts/2596601004.html","a8d9a0737a5577b396fe9fe1d26e32b0"],["/posts/2742438348.html","69cd25161fdf997e495eca8fb7ed2a9f"],["/posts/2864584994.html","efea5539f4b35457e757721c4f5f8430"],["/posts/2888309600.html","c5ded4563da60d1c9933d8458e7066ae"],["/posts/2891591958.html","b1f26ab78be38150428b0d5e1b9cdd10"],["/posts/2909934084.html","779dda3edd1dc0e7164cc3d41ae85e79"],["/posts/2920256992.html","01df789634f493f90c7c6415fa8cfb62"],["/posts/2959474469.html","13d517409b06900bcfff9b3c7c04d20e"],["/posts/3005926051.html","1017ff8d242576173b979889cf3613c1"],["/posts/309775400.html","aaa0c97072e493bea0eb331f48bbf044"],["/posts/3156194925.html","e0b19ddb469031983acdeb2bbf6703bb"],["/posts/3169224211.html","db729148830ea47e317efa6f3863c18e"],["/posts/3213899550.html","a7e114e230d8dcd33239445c63b6c01d"],["/posts/3259212833.html","56d6acde44ce8e418addb9dcdf7a223d"],["/posts/3266130344.html","0feac7903444dffe0f6c926baf077490"],["/posts/3292663995.html","33ecc9b405411fd6bb26f38e7c2597ff"],["/posts/3297135020.html","70979d449c92f2d69f81376b572bfc8b"],["/posts/3306641566.html","0cb6bb1771f5653ae7a5bfc3581e21a0"],["/posts/3312011324.html","b461e105b19d30f7affbf50d0616d676"],["/posts/336911618.html","99b3916b11847cc209847d8b085aa0cd"],["/posts/3402121571.html","b53627a4a2e05a69df1bbfef439a15ce"],["/posts/3405577485.html","d97beba29bccc3108b9164e5010bc999"],["/posts/3498516849.html","e6ca2b62756934731401041d2fb6a53d"],["/posts/3513711414.html","ac72360d8cd8704922a7c150bb65c834"],["/posts/3546711884.html","fa319c9d452e953ed5c60e5aa61fe199"],["/posts/3731385230.html","ca354edb0354e4c7c20f265c7aaaa6fd"],["/posts/3772089482.html","f53d0e86b0b718d74a660a97a6d9f7a5"],["/posts/386609427.html","29c52270ccc185fa94adcb45e3842d9f"],["/posts/4044235327.html","c532dd28cbfcd82fcd3c228736e2fe20"],["/posts/4115971639.html","58a6084ac6eb7c0628094a962a3f2413"],["/posts/4130790367.html","a59fe4e830cda303c5dc45f858621714"],["/posts/4131986683.html","98047dbaaa067f61a78eee097ff21d97"],["/posts/4177218757.html","4f5a134a3519ea469247baa7e4ffed15"],["/posts/4192183953.html","072b91771adbe9d66cea3ea5ab5b6723"],["/posts/4261103898.html","3755ce1b8c475bfa505aa21b1ed1a35e"],["/posts/469711973.html","5a557289eca32c0c8ec94836ebbaae51"],["/posts/482495853.html","f6d471b0d50edb93987695f11d6bb73e"],["/posts/488247922.html","ade7ec97d0ccac8967ef8cd0dfd3e412"],["/posts/517302816.html","23a0fd63e7b5440f06946693b495b74a"],["/posts/570165348.html","1064e204da169625ed769e61d6c18fcb"],["/posts/595890772.html","55f80ec5644dd10b71b9acfc9d9f25cb"],["/posts/67485572.html","125aef75468c6f03e53b72d381e81504"],["/posts/694347442.html","48761b28a2299fa3441de6fd78f721af"],["/posts/707384687.html","c80d1cc79853e8a03987bd620162bf1c"],["/posts/71180092.html","e267a177973d4badc80fcae52a139336"],["/posts/716459272.html","b52106427a35dac34bdf1f717c4be274"],["/posts/765481613.html","451f823a308e3c564814e577e676045d"],["/posts/778231993.html","e94290744c2d2d467fb70b38f1649c34"],["/posts/795397410.html","b9de6bf140f54b4e65c60fb083e97121"],["/posts/820223701.html","bd159019e986eea4e0294547bb06ee31"],["/posts/830372185.html","d254435dfa8efc3a28cad7e2149b6fa7"],["/posts/88294277.html","7c29d5a974daf3b0203a7af1cf715bda"],["/posts/939963535.html","bbfaf213f07ab9b760513d938b47f7bb"],["/posts/983786067.html","08d37eed9bcad1c334a530642e89a388"],["/sw-register.js","e08b07f84a1baa92b90581823f69ca82"],["/tags/C/index.html","fd19b7b5f5531fa0d6e30697f693f824"],["/tags/C/page/2/index.html","82df6f80d27f71efab9cafef1b467b6e"],["/tags/C/page/3/index.html","b73d38bd807dcc94923137d4cf3fb091"],["/tags/ETL/index.html","0e9f37824571496d0801edb03bb7e899"],["/tags/ElasticSearch/index.html","4155a8a49957c62d56fb40abddbe045f"],["/tags/GUI/index.html","71613ffe2bbb8bfe9029e10427febb43"],["/tags/HBase/index.html","f5bccc03f1a913e4aac3dae29f9015d1"],["/tags/Hadoop/index.html","3532f45b19a685a18dd975d7b18d12e3"],["/tags/Hadoop/page/2/index.html","0128bb654852703ae7efa9f50254b15e"],["/tags/Java/index.html","9b2d0e494f476b98298d87af7911cbbd"],["/tags/Java后端/index.html","835a22d4afa038fc5891c6ac2e7b8084"],["/tags/Java后端/page/2/index.html","7545e39323bc756402c2f08f0b1197cb"],["/tags/Java基础/index.html","244ecb9ce94242f23b2ae4cfd9a018f3"],["/tags/Java基础/page/2/index.html","522e16d55ca53cc3f2c9e889d4f3740d"],["/tags/Kettle/index.html","a9f7272db67b3b9adb93c1dc5c291bca"],["/tags/Kibana/index.html","010c9e0ee64bd78c0db6cfcac9d0d4b4"],["/tags/Linux/index.html","f712e741583c879d1e4642b6f4d8b95d"],["/tags/Linux/page/2/index.html","ccf64926ccfee223d0eff5981e9bb265"],["/tags/Linux/page/3/index.html","a6a02f643cb2721813e4e236f6adf6e6"],["/tags/Mac/index.html","fae11e82b7ee8a1c6834d8ef831059ff"],["/tags/Mac/page/2/index.html","8dbaa23e65cc1e3f1cb23042d69799d8"],["/tags/Maven/index.html","381dc49ec32106dfb9162bec31d8b8f9"],["/tags/MySQL/index.html","1be4a4785a0c7053b3d52ec73f814ed9"],["/tags/Python/index.html","77a93c404cf79ac5b42993d6d345e578"],["/tags/Redis/index.html","ef7bcc81e22cd0d0fbf13847f658f4b0"],["/tags/R语言/index.html","846e2a91ac0caeeb7f9a7d05cc11fe3f"],["/tags/Spark/index.html","1440f2ad6eea94733bb32ab607a4bbf1"],["/tags/Ubuntu/index.html","ddbf148fa27caa827caa16323f74cf46"],["/tags/Vue/index.html","cc54ec6d87481e491a90b2da0ba7e989"],["/tags/Windows/index.html","dc30c95e2240e61c743499341aacff70"],["/tags/ZooKeeper/index.html","ba4ec7f4c08e6b6e6857ada406d21ebc"],["/tags/bfs/index.html","e06fdc2d8cf6a297dfdd7a80dd687332"],["/tags/dfs/index.html","89361629f1ca40976c3c912f4ea5878f"],["/tags/folium/index.html","bb2765b03c71396353e0c702f19fdb72"],["/tags/git/index.html","7b1fa93638642ede7eb32d98cfab3f69"],["/tags/index.html","872179d8810ab4d29e5cd728d234bc0e"],["/tags/latex/index.html","7708bde53037ce8c96215b08992cb112"],["/tags/中间件/index.html","70534598fa69bc3257758593f1c6657e"],["/tags/二分查找/index.html","1d4f2971bc9194d7f9d3f544227dfc54"],["/tags/优化类/index.html","0e57df711f2cfd9a2b83e89227ef3779"],["/tags/前端/index.html","e6efff617fdb89e2d2483a2bf7dc1570"],["/tags/前缀和与差分/index.html","4226afc8d6ea01a5fc48aecd5a65facf"],["/tags/动态规划/index.html","10eeb596c9f615f9e735988d2566318f"],["/tags/动态规划/page/2/index.html","f41d593575d84d16bf3d9cdcf7bb7083"],["/tags/博客搭建/index.html","0a6ec9afb83b2a48b9198fe22c4231cb"],["/tags/图论/index.html","d63576b1d121bca8a30aaea96de055e0"],["/tags/大数据/index.html","6dcbdab2b687a68f91e2bec6b632fe91"],["/tags/大数据/page/2/index.html","8d8e83157304be622f021c9f9387a14d"],["/tags/操作系统/index.html","5c4d532b6449e98674f583031fcd5573"],["/tags/数学建模/index.html","2bcc0561e06b5705f45c8e250f631a2d"],["/tags/数据库/index.html","8761a2b313ec98b13a0e585eb8035df2"],["/tags/数据结构和算法/index.html","87aaf97aaae7e40581f7ebf13b5cc667"],["/tags/数据结构和算法/page/2/index.html","2e8b60129f1959da82e0c7518a6d9f61"],["/tags/数据结构和算法/page/3/index.html","6fb77026deeb3f36681515d24c3346f3"],["/tags/数组和字符串/index.html","fbc70cf2948f4df7a7003baa353a86f0"],["/tags/枚举类/index.html","377daa37eb16b5462501f4f73e942307"],["/tags/栈和队列/index.html","665ea52c7b992b2a379d979f87c2cfc8"],["/tags/树论/index.html","fc6329a9a2a1cc6f08e6719dd306864d"],["/tags/测试/index.html","41890be35f9ee7cd67f685e2615eba79"],["/tags/环境/index.html","3c7e004650d86add3fa23bcb26e8181c"],["/tags/环境变量/index.html","77930c86c591636887cec7090442982f"],["/tags/绘图/index.html","3279cc2d46ad1d7d7662bd28e28f60be"],["/tags/编程工具/index.html","f54d653d9761c020fd298cc77dbaf62f"],["/tags/编程环境/index.html","f154db9b2860684042469b7b3a67354e"],["/tags/网络编程/index.html","2b4b4341067caed3eaf7cf21839af0f0"],["/tags/英语语法/index.html","1364d59436b38803f73499a152f80b1e"],["/tags/计算机操作系统/index.html","bb8b79a0082825fa6aef2de2316460e4"],["/tags/论文/index.html","2b9baad281e0dd25f69a0210eba331b5"],["/tags/资源下载/index.html","d01767d4826ef72e7889beb7ea738a13"],["/tags/链表/index.html","e72514d76cdeb763bf6da7e5084de0d1"],["/tags/集合/index.html","769646a6d6a912f5374b10e8a6443ff4"],["/tags/集群/index.html","51419f02556fac3cba06227905109bb2"]];
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
