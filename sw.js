/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","9fa48edded891ae96081f687fc5aaba2"],["/about/index.html","01b50240e45dd62031867103ebd6cf13"],["/archives/2023/01/index.html","8aebfff993d9b5974175fc95effa09ee"],["/archives/2023/02/index.html","307e0e1ae1504a71ebee23552ea4652a"],["/archives/2023/02/page/2/index.html","39018440ee6968a1841f58f4785c3984"],["/archives/2023/03/index.html","fa8b3576dff470bfeda05b8c671790d9"],["/archives/2023/05/index.html","98c80ca45743745685c4741419dbb75c"],["/archives/2023/index.html","0716f7dfd1bce77802e7157a6b9dc4a5"],["/archives/2023/page/2/index.html","a7781ad8046d005b23ac0056745d01c3"],["/archives/2023/page/3/index.html","cb99538442c1cf0df2dfaaa4790ffd7a"],["/archives/2023/page/4/index.html","9113480261dffc5108686863f23396ad"],["/archives/index.html","58681fa43c9518f0ab89ca3a3a06e620"],["/archives/page/2/index.html","c04faf91851d092c34219e81bf3def9e"],["/archives/page/3/index.html","b0f0b5430ef3c4a78574dc1ccf0a0797"],["/archives/page/4/index.html","325b735b16714c44b38ec1ce83d4042f"],["/categories/Java/index.html","d89f8c6e7543ddaf88d22c071ddd7b96"],["/categories/Java/后端/index.html","c17ebf3097a938972f66c5835a9a638a"],["/categories/Java/基础/index.html","268b5f76a6d7d32a2dc803acd11b1611"],["/categories/Java/基础/集合/index.html","11edff89f38d5fa753630547b16e6442"],["/categories/Python/index.html","eacb5956389178a6c4309ae9a782d013"],["/categories/Python/编程环境/index.html","2f917c4dd3af31e5644e02bc44e9897f"],["/categories/R语言/index.html","102ee06703b7e883cea973242ce07708"],["/categories/R语言/编程环境/index.html","3d2631c8a7adf544959a2689cc05c6e2"],["/categories/index.html","018c8e2efd430fa0b4aa48c8056be480"],["/categories/中间件/index.html","8716580b6a16da17ee17b8130afb3a51"],["/categories/大数据开发/ElasticSearch/index.html","f9d1134101dccb486de8ae8d49d2716b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","c08836f1ef261dea4284305ccc3f67e9"],["/categories/大数据开发/HBase/index.html","2b7e5cc0b21cda52a605cecf48e07cbb"],["/categories/大数据开发/HBase/学习笔记/index.html","35eca8d65b775fa849925906a5db5da8"],["/categories/大数据开发/HBase/环境搭建/index.html","9a861ae33446482ad5f95061ee420ecf"],["/categories/大数据开发/Hadoop/index.html","4d397ead2f767f8514760f37a8e22bf7"],["/categories/大数据开发/Hadoop/技术/index.html","7d20408c6ca2b3b5f838a869a572509e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","54753a552a81728179e76f753d043056"],["/categories/大数据开发/Redis/index.html","37b939f6a8c2b586c87ea4137ec391c6"],["/categories/大数据开发/Redis/环境搭建/index.html","64bd58118eb6cf4b5b152d9dc20cc8fa"],["/categories/大数据开发/Zookeeper/index.html","7bf4303d2da85ffd5e7d69c75d77dc1a"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","bbd1632eeca2a36d2504adc0a179ba93"],["/categories/大数据开发/index.html","178822a9fa65fc2f9ef27b6bfb0f7036"],["/categories/操作系统/Linux/index.html","ad01ad0dd535d43e4fa7a91810dbf3f4"],["/categories/操作系统/Mac/index.html","754cc1b8280f64d23a2785db9353ccc9"],["/categories/操作系统/Windows/index.html","ba95505f77cbe8a99e919c9cc0ef6e20"],["/categories/操作系统/index.html","e48641cab966d8e615e2bd84f3fea809"],["/categories/数学建模/index.html","4fc93bc4e551372ae3e4063c4e30ef07"],["/categories/数学建模/latex/index.html","3bde4309e3e3bbc3f07d194b182c17d3"],["/categories/数学建模/优化类/index.html","5f003fb719f88a8d6442123335e7ebce"],["/categories/数学建模/优化类/现代优化算法/index.html","e88c120e776a82fd416f5cc9a775024d"],["/categories/数学建模/优化类/规划类/index.html","1985ea98f5e208ab6fb0a714afc356fa"],["/categories/数学建模/绘图/index.html","be3f3334b6c511504b4fe252536b8468"],["/categories/数据库/MySQL/index.html","3e9126919217fbe422d8e995bfeea54e"],["/categories/数据库/index.html","2ef59bcee4487b6903251985441294ff"],["/categories/数据结构和算法/index.html","3aa2c9472ccffeb9565a0199be5a6d07"],["/categories/数据结构和算法/page/2/index.html","d56a8d4db85496880235ed51a4f22161"],["/categories/数据结构和算法/基本原理/bfs/index.html","ed0a19616578dfad0b652b69d89eb05d"],["/categories/数据结构和算法/基本原理/dfs/index.html","76a09f871ea539f1b7e43d25e2bd84a8"],["/categories/数据结构和算法/基本原理/index.html","8b958ee0fd9fdca25e7cc109aa17ebc8"],["/categories/数据结构和算法/基本原理/动态规划/index.html","e8f5301cd4ae256ffce8490575c8970d"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","749acbba52fa78ac4f9f12f2ff829c7e"],["/categories/数据结构和算法/基本原理/图论/index.html","06b83a060b8ef04d43f9c70c7a7d43fa"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","584feffddc420f45afb5174ce4272520"],["/categories/数据结构和算法/基本原理/数论/index.html","cf779db3bf1ca837c4cdfc98d6339228"],["/categories/数据结构和算法/基本原理/树论/index.html","8c5405371f916e86cbf147ed7e5e85c2"],["/categories/数据结构和算法/基本原理/链表/index.html","21ec510a1912afbb375f016ae5224634"],["/categories/数据结构和算法/算法题/index.html","739e992bad85d31dd8d5d84c6c7e3a74"],["/categories/数据结构和算法/算法题/二分查找/index.html","4abb4cef84a545a5c8bc6bf93fbb3031"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0dd51ccf974fa43e01ee5e58d9c0fabc"],["/categories/数据结构和算法/算法题/动态规划/index.html","5787257ef7807682db9a233acc157a27"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","c5417b69f2697096a770d389966f9b45"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","5f7bd5aaccf97b949f36ad6d641f5349"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9e60215f2412ffdd6e10463b0a1b9a29"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","ff8d74149bcb8926ec905f25324d694c"],["/categories/数据结构和算法/算法题/栈和队列/index.html","eca041950cc132ea4e8e2ffe451b5943"],["/categories/数据结构和算法/算法题/树论/index.html","7b0f69bab38ca869dbeaddb50848bdc0"],["/categories/杂七杂八/index.html","1ca52049c6ca62c415a8b22a1791c9b3"],["/categories/杂七杂八/博客搭建/index.html","76c00a08b5ea0954c59f6077dd994084"],["/categories/编程环境/index.html","af61c1df192e8e8fe61ae24b6021a24c"],["/categories/英语学习/index.html","9d2a040ff427035889e65d04a616d269"],["/categories/英语学习/英语语法/index.html","40b280379ab5ae23d2b889ca0a71852d"],["/comments/index.html","a48f7ccb632e419776b43fa81f226fdf"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","96e19abe07c860de839213c2cf19a180"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","56f2b55c347a2d125fe3d880db50d93d"],["/movies/index.html","437d4b66adb2e664c61ae1232211d97e"],["/music/index.html","800a53723377892e138d7ec4f891b64f"],["/page/2/index.html","81e6e9210b95ef64dfe1f1e98848dad1"],["/page/3/index.html","8b29e10f30fdfb1b56da1910acd0e475"],["/page/4/index.html","5992a2cf9a43cb9a7f01a53badef0f1c"],["/page/5/index.html","223337455d43c760b4efd2be3a609033"],["/posts/1021360842.html","9406b9bc6f58e4298cd3e5d6bf66a6de"],["/posts/1120620192.html","81cd20f8dadee7964ac6773a7dd7f6c8"],["/posts/1141628095.html","db7e7920e2aa2f9e9551efb162300dd9"],["/posts/1168613674.html","13fc4e4555759645b3b2a087926b3848"],["/posts/1219920510.html","c4506faa15f26564d17b62eb5037c29b"],["/posts/1222166338.html","d281cac6ac87a373485e0400488c4e69"],["/posts/1259097482.html","a6e9bbd3b5b79ab0081bcfdfde2e9eb5"],["/posts/1271036369.html","fcd0de52db7f2d47537063f350d86097"],["/posts/1312847445.html","cbdd1257ba52e86ca849ae92b4593906"],["/posts/135355774.html","f0bceee616b1c687299cf83a77fd92cc"],["/posts/1375344716.html","ac52edbd431a94bb32709a80d49ce6dc"],["/posts/1388991698.html","d97b57d359674eaff890a254f9d6d086"],["/posts/1410315814.html","9bfbf111f1842537fe30af48174a127d"],["/posts/1452790229.html","ad080a21f8c4b328313a1614f06ac831"],["/posts/1470079884.html","dd795c90059e88b3e97fa660a5ba691b"],["/posts/1470079885.html","a633f69ae44b808f1000cd98a3c3b480"],["/posts/1470079886.html","f884a59ec8ce2afb0f53cf1925b29e7f"],["/posts/1470079887.html","917b0f243e5c0747a8777d651ba533ec"],["/posts/1498536549.html","2eb6e270f0355f647f0249bd76b6d572"],["/posts/1547067935.html","7cdb18f8299131e3998fea6adb3f9fcc"],["/posts/1557866301.html","fe3e83cacd01c851f9e586271c975195"],["/posts/1571776361.html","dfa93e3823938b50deae7304b21c70e7"],["/posts/1605124548.html","8aa4e2f196c447fd4e13a16233bdaeb4"],["/posts/1633036852.html","26b23b60330cb1f59f698825ffed2c3b"],["/posts/1765123828.html","180785db73540e97c9e5ee8622601d78"],["/posts/1767336200.html","90e7761a69d6b9db9a521fde4c7924e7"],["/posts/1776114197.html","f74ad77bad036b402b6ef46aea94f7ea"],["/posts/1817748743.html","95d05706b95679f9bd82600be7a5bee3"],["/posts/1925125395.html","7913abe07c2e186d022fa50197fee569"],["/posts/1966191251.html","f1bffd1a3643c5a2f5e7523effab78ad"],["/posts/1987617322.html","3c9415acece0379220a05b4510cfa516"],["/posts/1999788039.html","4c56c1288de62d252be4ef0ce10bc724"],["/posts/2075104059.html","4080ce826314a004d1a4b6be897b9ebb"],["/posts/2087796737.html","653df4f4c1092c7878174e51e934d66c"],["/posts/2106547339.html","c89aca7155cad50cf210940e6c38dbf6"],["/posts/2207806286.html","da4eacc6ddd5f3540d01cb339a94e464"],["/posts/2225903441.html","79534e0b62103441981677045a701fbe"],["/posts/2265610284.html","10a81045382b2c9307d213bf31caa6b5"],["/posts/2281352001.html","b951d0035a8caffbcda59dc65b51c088"],["/posts/2364755265.html","3587254e31f68a49cf0d4a2066878884"],["/posts/2414116852.html","fedefb8f66879c7779ef3ec8b997a506"],["/posts/2482902029.html","9a0af9a6fe241b2335a712a2e3a72c06"],["/posts/2495386210.html","981cfa8697311c7eb77ed4b5ba750faa"],["/posts/2516528882.html","0f250e3871e28e6ff36ce825e5736803"],["/posts/2526659543.html","458569b833e67a09b9286d0e02b3f636"],["/posts/2529807823.html","6ed6337e60e9f76d50a4ea55cb536d2c"],["/posts/2742438348.html","e90572c3e285a45f9d11979e066b4600"],["/posts/2888309600.html","2bb4eba243ac148e27e23387c339a6c7"],["/posts/2891591958.html","38d767b3f86ecc04114f8ce2f4142eec"],["/posts/2909934084.html","5dcdd8a8bac96ca0737325b29f871a54"],["/posts/2920256992.html","c51341534b2bc91def8373f4f687c691"],["/posts/3005926051.html","daa8822e1186ac896f3c2cbeb202ea74"],["/posts/309775400.html","e91d8476c3804411dbdbc50142f73cd3"],["/posts/3169224211.html","a63633f9ecd27054ae901e2435e988fa"],["/posts/3259212833.html","7f8a77ae74e550887df189ad572d3b91"],["/posts/3266130344.html","506062896d27167e89be0804e50b5dff"],["/posts/3306641566.html","fab34687d40b9b7cbf5f0cf10e8b21d2"],["/posts/3312011324.html","77e2a6e4c58a4b3e0ac35891a8b20902"],["/posts/336911618.html","1575f582fb2b4e1dd35d5c78a3f7085b"],["/posts/3402121571.html","8eef07ecd7f49124ee041339b7dd4b33"],["/posts/3405577485.html","bdbedfb8a2cdcf49355966d357db0b92"],["/posts/3498516849.html","383256d3ae5f554eac50bbf2facba8bb"],["/posts/3513711414.html","4308c9f5af50de522a12d7937fe5db12"],["/posts/3546711884.html","f343c81c6965fef4a3e2278a014dd6f3"],["/posts/3731385230.html","1bcd2267ab08311f061d7481be03047e"],["/posts/3772089482.html","e36b2452fa0d96c71a66e898ae54ef06"],["/posts/386609427.html","4ecce1713dfcf2c828bf53a9ea19b3ad"],["/posts/4044235327.html","d50f741e014dc9b0f855622edeb15413"],["/posts/4115971639.html","1fcad811b715ba25298dd2c7752cf9e0"],["/posts/4130790367.html","a96f28c2b42b9d445de6a59d140d5b4e"],["/posts/4131986683.html","402e567f18432c8b8b870a1b9a1782f0"],["/posts/4177218757.html","6cbba9bf85790c0721a216369f8b6377"],["/posts/4192183953.html","37ad6322f86b49803c2ac4f786600d4c"],["/posts/4261103898.html","6e6df15f75f7db90d6d85cbdbc942303"],["/posts/469711973.html","05541147bc58c96dd488cb99abb5f22d"],["/posts/482495853.html","e46687ae6168df58a7b50abaac38bd19"],["/posts/488247922.html","cf8e5c7e732b5df45b6aa6af76d57ef2"],["/posts/570165348.html","c1a710f68a12af57030b99b3d4c39785"],["/posts/595890772.html","7ab7080a2b174e85d7860c12df5d7480"],["/posts/694347442.html","39c20b515f33da3a39efeefc9b0e0a30"],["/posts/707384687.html","538cba0cccb584386328b63ef70dcabd"],["/posts/71180092.html","270d6900dd50863b6cb4077b734f4215"],["/posts/716459272.html","3db4c0f399a5b09d7079cbe6f161ac84"],["/posts/778231993.html","4d850ccdcbb6cf9516ce442c733c531f"],["/posts/795397410.html","64e4adc062d015afa6a5488a627bd7dc"],["/posts/820223701.html","a8f8f0ec4d8021ec40c32397ba142242"],["/posts/830372185.html","0c01058988a1f258cd0d7daf73fc4b6d"],["/posts/88294277.html","dd0a9affa227c43843b770f4da7e7461"],["/posts/939963535.html","22b8197f10724ab373db54bc5353ffdd"],["/posts/983786067.html","0709690bc6b23751037240fd464056e0"],["/sw-register.js","b9d9957d0ad6a502fccefa78354c0036"],["/tags/C/index.html","dc07b708aeb285f7d3bb95ac95d8d6c6"],["/tags/C/page/2/index.html","8c14605c357ad1d67008431d480a70df"],["/tags/C/page/3/index.html","692006bca5873224c851db4cddd5ea83"],["/tags/ElasticSearch/index.html","7693aa0caa6ec734e5b21a1f58d8092d"],["/tags/GUI/index.html","3f76d06c0e1a41eaf0f1d56c0e81268a"],["/tags/HBase/index.html","b2ecd88bd6ced0fe38d5c27a0356e4b4"],["/tags/Hadoop/index.html","3621d4819b7eea385db043900ff13239"],["/tags/Hadoop/page/2/index.html","3eb4bcb30b2e163e201d73ebd83efcd5"],["/tags/Java/index.html","53e994e53cc452f31e729a32e1d35f2f"],["/tags/Java后端/index.html","2d37d8c73b08aaada7a6f6d339d02fa0"],["/tags/Java基础/index.html","e850a82f9771298d807ec8501795bc81"],["/tags/Java基础/page/2/index.html","eda0e20bd44625a1db9117215f0265be"],["/tags/Kibana/index.html","e485de97adb51f0d0301c868a21995d0"],["/tags/Linux/index.html","59a5186dd374d89fc40e762745872205"],["/tags/Linux/page/2/index.html","a7309caa38b8bb170aee78d7a549bcf5"],["/tags/Mac/index.html","37d16eea65809520ef56d52fa51a9e4d"],["/tags/Mac/page/2/index.html","ef68b9116f997cdfb5fab44a0d5ffa95"],["/tags/Maven/index.html","e96b8106d8441d193e89b29758dd1495"],["/tags/MySQL/index.html","4f6f0a3207af867530dd22752686278f"],["/tags/Python/index.html","9ecbf4f5a6b13d8e04ee6c30a43bbaf2"],["/tags/Redis/index.html","27fe53efbc6c430139b20bdefdd03242"],["/tags/R语言/index.html","add3ff39d15129897e2d2b7e6eda819e"],["/tags/Ubuntu/index.html","d84fc2b61622091b1a2e9067ae593d7a"],["/tags/Windows/index.html","c76e1798ae6d86df88d3792c25df2248"],["/tags/ZooKeeper/index.html","c8085878fcaab3385699d5fdd987f7ac"],["/tags/bfs/index.html","de66fdafdd2665c3d99554a919bfb635"],["/tags/dfs/index.html","93701396ce3f1c2c64887c1535785216"],["/tags/folium/index.html","ddd09c5fe6df73c8721da4a2ee56f858"],["/tags/git/index.html","ee4ef77e5bff2e6b7bcadf1ecdf5bb4e"],["/tags/index.html","5ec7d6674eb9749eb5134fb8bbe2065b"],["/tags/latex/index.html","b165b3486f9e547caf8f1a7e421d3272"],["/tags/中间件/index.html","1156b79f020c071b8f28cefe89c017f6"],["/tags/二分查找/index.html","506e1b4f23a391d4d42d3ae4ecf06278"],["/tags/优化类/index.html","14d68ac3f84d18ae9ce1a97b3214a910"],["/tags/前缀和与差分/index.html","64f90741c9ecbf45ab3f920597524875"],["/tags/动态规划/index.html","d4a1d78466c113ca02af42c20ba42186"],["/tags/动态规划/page/2/index.html","801473ae16f2cda0769bb7bd5ea31d8e"],["/tags/博客搭建/index.html","74645dc44d74ae3845555733f761c046"],["/tags/图论/index.html","b95ad1fb0e545018cd78a7d090f9de48"],["/tags/大数据/index.html","652183095d121b17e466d59d9f95283f"],["/tags/大数据/page/2/index.html","4a73296bd4353450c676018eb2459c9d"],["/tags/操作系统/index.html","1d3377b382de13d95aa690a0746961ba"],["/tags/数学建模/index.html","c54cacc9d934646e7a2d0ee4d16d6aed"],["/tags/数据库/index.html","349fe7da16d28080e8a22887bdfb0212"],["/tags/数据结构和算法/index.html","e1d347379401f4542961cc10ba43f46b"],["/tags/数据结构和算法/page/2/index.html","d7551512c38960dc20833abf26f1820d"],["/tags/数据结构和算法/page/3/index.html","d72737c1573ba11b5ef13381978f9584"],["/tags/数组和字符串/index.html","40c75a4f32c911b6eb3668134fd4d79d"],["/tags/枚举类/index.html","bef0afe8d82f823cb5da4be0ab02e5bd"],["/tags/栈和队列/index.html","79924ab14b44a8760a49eb26646d9905"],["/tags/树论/index.html","a773f01fe27af0588f16132687f281e1"],["/tags/测试/index.html","8c6464ccfcbf245c3d047255d5d41a32"],["/tags/环境/index.html","b131111a0897311887a56534b8b6fd6a"],["/tags/环境变量/index.html","fecd4fcbd43e0324d2d706f1154fc2b6"],["/tags/绘图/index.html","ff76dad63c12929e2795932a728617ff"],["/tags/编程环境/index.html","52d5d71aa7d599335d05d1853fbdd220"],["/tags/网络编程/index.html","179aca959db7c38e21dd5d7745bf108b"],["/tags/英语语法/index.html","b13b2f71cea88697e85063d909301de3"],["/tags/论文/index.html","5b5315f93c552a4f0517562cdad16ab1"],["/tags/资源下载/index.html","0306649e191eca4c58f23b30c69fa3c4"],["/tags/链表/index.html","f91eb400b275ce49f9415b9608fb813c"],["/tags/集合/index.html","6219162647b44bf53ad821fe5fcc7b07"],["/tags/集群/index.html","d8eeac2c30c27a2f03d899437e3f1a6c"]];
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
