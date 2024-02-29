/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","a0ae28938294689178627cfb0a6328df"],["/about/index.html","8b27792c62c63312f6cda047e50d3e40"],["/archives/2023/01/index.html","0ac727695929a1dc0560cbaad19a2056"],["/archives/2023/02/index.html","efcc82774c9c19046fe1832235ab0c68"],["/archives/2023/02/page/2/index.html","5c94d11433f864bbf6e569a194ab46d2"],["/archives/2023/02/page/3/index.html","7c6b01a44c218513814e99cc9c9fae0f"],["/archives/2023/03/index.html","4941686de4aad069143dd85e43e5a0bb"],["/archives/2023/05/index.html","1071ebbfa579f0615dacf3189c4a6344"],["/archives/2023/06/index.html","2d8621c09a20d69ebf54df45685bb773"],["/archives/2023/09/index.html","f23b73eac7225529784de838475d27a9"],["/archives/2023/11/index.html","4cd41bcc64b999f2227cff778d46fd30"],["/archives/2023/12/index.html","125fa8eacaa56ba92822f378a8f1b300"],["/archives/2023/index.html","954c9de3b34bfc566e0532eab5fe9b89"],["/archives/2023/page/2/index.html","991de921347046a0c25ee7f1e12987a7"],["/archives/2023/page/3/index.html","4b197abe6176af7c7641f9f03d2af1fc"],["/archives/2023/page/4/index.html","94503290eeb91486b97fd8f93b021781"],["/archives/2023/page/5/index.html","cea10adf851d39b4e2ad4e801092c1f0"],["/archives/2024/02/index.html","6b7d8e062a3a3cb50374d72e0819f1c4"],["/archives/2024/index.html","59b3ff26615f0359016d73c38ecf2967"],["/archives/index.html","dc77cc6cbf2064a9ad99e29d0696f83c"],["/archives/page/2/index.html","85b0d20da715a9453f4b990622a3aa4d"],["/archives/page/3/index.html","9ff5c40910c0d576ba6c75e0825fa712"],["/archives/page/4/index.html","9c3ce478fc61c20683a3ae7ed94bcb5c"],["/archives/page/5/index.html","580f772f3b9a91e3f7268a3d71354578"],["/baidu_verify_codeva-qQP2iZOMLX.html","27a27adae3a3cdab0aedd519ff2ce41c"],["/categories/Java/index.html","c6879abd388110b17392c8b11c1c303b"],["/categories/Java/后端/index.html","530e1e75140b17fce4ea24a011edc035"],["/categories/Java/基础/index.html","467d23282e09260718ea02b328479b2b"],["/categories/Java/基础/集合/index.html","656065a0bc7ed2fa87631d8be2541806"],["/categories/Python/index.html","6919d92ffded9396be5fd24067c83fa0"],["/categories/Python/编程环境/index.html","0edab5d3b425a36a3c0b64cf3c07f3e4"],["/categories/R语言/index.html","2732759f067582d27f052554f2a29367"],["/categories/R语言/编程环境/index.html","b9148d7f865e224827f23dc0bbe6a0ef"],["/categories/iPad/index.html","baa8c79daef51bca63abb16853be6096"],["/categories/index.html","4f22613fc0ee8d04779ffb94c70ba029"],["/categories/中间件/index.html","5268366349c374dc6d5660b4b5b19a42"],["/categories/前端/Vue/index.html","1d4ccd641fc37910345e4cd8b03608de"],["/categories/前端/index.html","7209ed264ce4cbdab53b526d72a9b6cb"],["/categories/大数据开发/ElasticSearch/index.html","3b73f3c2c65089b6e3cc2f27ad0cc020"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7b9ad14a1317027cc28f56cba6645a3a"],["/categories/大数据开发/HBase/index.html","87ec3913a2e29dab99538baccb5e3093"],["/categories/大数据开发/HBase/学习笔记/index.html","68ff0add8933ef44040cfffda9e97921"],["/categories/大数据开发/HBase/环境搭建/index.html","0049304818dc1482cb647a8f8f65541f"],["/categories/大数据开发/Hadoop/index.html","f79bf5f4129d75b6675f5cd657262f26"],["/categories/大数据开发/Hadoop/技术/index.html","76e54e40bd847b00a72c141f51424fc8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","9f64f92e37d0a864ad3bf664e9c37ae3"],["/categories/大数据开发/Redis/index.html","3d2c5cd085b42f863fce7bf8a6d44a3e"],["/categories/大数据开发/Redis/技术/index.html","29b4381cbe665351f59682fb86a795e7"],["/categories/大数据开发/Redis/环境搭建/index.html","aa23aee2e24c7d104d017b05b7b9982c"],["/categories/大数据开发/Spark/index.html","76ce55a75dc8a710b901d23046a3c7b9"],["/categories/大数据开发/Spark/环境搭建/index.html","0eccdf37ed8dac8d02944e84ad51deed"],["/categories/大数据开发/Zookeeper/index.html","a94ed316aba6069b286c4310e63db84d"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","6f2c6e20e9256b5c3ae6cae71da6c14a"],["/categories/大数据开发/index.html","b5bf4abd25d605a1aad5c3f62a87b9e7"],["/categories/学校课程/index.html","2500c183a9144d2022864c4c684ff6c6"],["/categories/学校课程/计算机操作系统/index.html","7050e1e8e73b27010f58cb2f48a0bb19"],["/categories/操作系统/Linux/index.html","2ed1e6f525215d85d0cf5b2f718435e6"],["/categories/操作系统/Mac/index.html","c23186b1838455d046153b1c802aa26c"],["/categories/操作系统/Windows/index.html","39c9d755e87b067b4bbcc70430482683"],["/categories/操作系统/index.html","4127a673cb60e6efde860543819b7519"],["/categories/数学建模/index.html","da40830a1624aca2eeb3f9cb5fb8353b"],["/categories/数学建模/latex/index.html","cd7f4f6fcd2e333767000301c0b4df5e"],["/categories/数学建模/优化类/index.html","3b1dba8daf1c95e46d808517461d377e"],["/categories/数学建模/优化类/现代优化算法/index.html","7ac51e78d68db12bb48b95b1a3aed829"],["/categories/数学建模/优化类/规划类/index.html","b690420323a13c48b74588463e84a052"],["/categories/数学建模/绘图/index.html","86bedb1c3d3c361dcfc88b0e87367645"],["/categories/数据库/MySQL/index.html","e97394ad7e855d3b9e520f73e70c834d"],["/categories/数据库/index.html","79691236bec8878c6a1ed4ec3cc06397"],["/categories/数据结构和算法/index.html","a9785fad8cc0477aa63df5a627437afd"],["/categories/数据结构和算法/page/2/index.html","c66e0aa0185e35640557914cb779781f"],["/categories/数据结构和算法/基本原理/bfs/index.html","adedb5f0a2ef69ed0ae2a4ce6adbde42"],["/categories/数据结构和算法/基本原理/dfs/index.html","fe515580119d8842896c80242e15133a"],["/categories/数据结构和算法/基本原理/index.html","d42592bc2bebbf352d1c318f89e4028a"],["/categories/数据结构和算法/基本原理/动态规划/index.html","eda54a56afcfd5e3b7a6bfd72db36c64"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","5a73e7cc002e408e79c4a12414b53306"],["/categories/数据结构和算法/基本原理/图论/index.html","8b66fbf41637de3f1f629aeccaa30af8"],["/categories/数据结构和算法/基本原理/字符串/index.html","9f2e9396b61906839940a6bd200e8ed8"],["/categories/数据结构和算法/基本原理/排序/index.html","59abfe78dfd38f132b32f23116dd2726"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","fd5919d3caaaca4cc8c7f5b5156e9c9f"],["/categories/数据结构和算法/基本原理/数论/index.html","256e17429e8aaa9218507d5e5f5355a9"],["/categories/数据结构和算法/基本原理/树论/index.html","87769c4a5a908f5e9f95fd26ae11c282"],["/categories/数据结构和算法/基本原理/链表/index.html","8347af62fd59ad341ca649bb0e1c37f3"],["/categories/数据结构和算法/算法题/index.html","08f10d932f2c478767f32007c71f294f"],["/categories/数据结构和算法/算法题/二分查找/index.html","d9418bf62d52a7b5c83f164833656741"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","1a9b15e2d166fad6adec73e64cf90315"],["/categories/数据结构和算法/算法题/动态规划/index.html","e62e17dec46e020e97892dfd6879d62e"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","d6face5d3e9a83d129ab523e50403055"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f3277a56d820072a3f29a926afdd1c98"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","1bbde562e1e2e51aabbea0a13120847e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","aa91f891b13477a8641b0313e1c4408d"],["/categories/数据结构和算法/算法题/数论/index.html","f93cf6022c271a7c6f6fb55d137298c9"],["/categories/数据结构和算法/算法题/栈和队列/index.html","db8610761189f79c65285d39e7ed24e6"],["/categories/数据结构和算法/算法题/树论/index.html","e2667473712a28a9511b26422d3012fd"],["/categories/杂七杂八/index.html","7f14dcfbd42567132da54a05ce2d01db"],["/categories/杂七杂八/博客搭建/index.html","4f6d2ca2048d94a94789377c9effaddf"],["/categories/编程工具下载/index.html","dcb54b39f0c446f9d19c53d16fb80235"],["/categories/编程环境/index.html","247e073bf24ebf67872cc30830545bb9"],["/categories/编程环境/大数据/index.html","2a61cfbbc459456daa5322d3a33698ce"],["/categories/英语学习/index.html","1cb1684e60b61ba5557086e2a7823a99"],["/categories/英语学习/英语语法/index.html","878808018f525521e5682f269c98324f"],["/comments/index.html","e3a93772f2d4fa85d45af145186f11c4"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a15bdb047c3bc10f46dbec7466a45b55"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","89d3dd2f2536bad558026ef79932d8b9"],["/movies/index.html","b39d3bd979f2f4710b2dc1fb4ffca703"],["/music/index.html","9548118f9cf6ddf14a94f374a00334bc"],["/page/2/index.html","9f43fdee8aaaf45043c7eed711af5e08"],["/page/3/index.html","544bd145a54b056c3342dbe0a8d3360d"],["/page/4/index.html","1f9994f437174232067e960df2b15215"],["/page/5/index.html","be8434fcd23ded727307f6068b67bec4"],["/page/6/index.html","8badb1e4929749188160f1f265911308"],["/page/7/index.html","0ccc519423f6236353a7c9ec503e52f6"],["/posts/1021360842.html","bef999b4a4734d3f189b0fa815349a21"],["/posts/1120620192.html","08d61e34f17d4c06ac1a093eaf6a7d24"],["/posts/1137707673.html","66feae45a35c3995ebca5727bb1bbc20"],["/posts/1141628095.html","744e7fe07bf2d22bb20a63a8b2efb67a"],["/posts/1168613674.html","b57e6a3d0d11663c5b9f91fc6b871902"],["/posts/1219920510.html","3a4ebb78d17d8b45855bf2a1087f5736"],["/posts/1222166338.html","ca36c97bec207623a586c859fbb66dab"],["/posts/1259097482.html","e761ce2cdbdaa99f6e9ce248fc6faa86"],["/posts/1271036369.html","35a7aaaf140b1b185d06e23e7652a339"],["/posts/1312847445.html","35c1f03e825ceb6a565599a5dea6a43e"],["/posts/135355774.html","8e572bd7ed8b56148a9832b92a0caacf"],["/posts/1375344716.html","82dba6a31dbd7a5190d84882b7ad2492"],["/posts/1388991698.html","0360a63f39e552044d5761ed5d42ce21"],["/posts/1410315814.html","ae496ac8e7cdb0937c3ce0e9c0f070dd"],["/posts/1452790229.html","10aeef3baa8e81688232cf7ec5ad1bcb"],["/posts/1470079884.html","edaa786e20ee82845b7e5b65e1cafeaa"],["/posts/1470079885.html","04be6f6586c8a3ca0a232ecf7bb99c7e"],["/posts/1470079886.html","783762e7990af7875efdb36ba59f08f0"],["/posts/1470079887.html","0d90e0ad1b67e8ffa9b2fffac7e51dd2"],["/posts/1498536549.html","e0d94d92bfc107a86d31e06343bd64d9"],["/posts/1539568593.html","0a43a269b188f49492b8c281a2075736"],["/posts/1547067935.html","671f56c6bb1c697728f12b9d8685b73f"],["/posts/1557866301.html","9c1d8a555f9b32c0dae834d8d2599a0b"],["/posts/1571776361.html","8b68da0dd7703e4e1e4e961859df6832"],["/posts/1605124548.html","c5a0b0d4d27cddbcc5425ec8b7b48ae5"],["/posts/1633036852.html","cc84ce52a864db279b8dcd3db015fd03"],["/posts/1667740714.html","fc358dbe2600fe29138d748e2875d87e"],["/posts/1674202625.html","861af97a79dbeb0420a65fb90f8ebaab"],["/posts/1765123828.html","7613b262119a469fd4d76a0f1d696399"],["/posts/1767336200.html","9381b610f41f2b1431cd5763732172bd"],["/posts/1776114197.html","c1437fc523d52f5df7b5bdcd8c63abc5"],["/posts/1817748743.html","d4f72f9c65b798d3ffe88828c9528b02"],["/posts/1925125395.html","3eff27f0c3c6e75ed079bb89ef03632b"],["/posts/1966191251.html","a013fce1092fede329351effb43e6473"],["/posts/1987617322.html","2440f386a155e74c67be5f28bd78714c"],["/posts/1999788039.html","3d3389b9bd20b1cb9fd1d194014d2f8a"],["/posts/2075104059.html","839134eddc65d9394a1bf03c57875f40"],["/posts/2087796737.html","4bbeaa1d39c2ea9fd70ca3ebcada4499"],["/posts/2106547339.html","e93c7558c8a7395da5c878dca5f3b907"],["/posts/2207806286.html","88fe96e8054f3ce813b28648c52cd7f5"],["/posts/2225903441.html","b8dbc5f67104e7f4ae22359476da62d0"],["/posts/2265610284.html","8e3b59dd75382b393a6c9a9defb87180"],["/posts/2281352001.html","c1fc7a42160d1d0e786ee3de8d08680c"],["/posts/2364755265.html","af2f8ae96349f2991947b1b7e1a4a4f4"],["/posts/2414116852.html","ff710425ea517301c6c8b60c12364cee"],["/posts/2421785022.html","e6c8285d05db75dcfe554b4ec02829b5"],["/posts/2482902029.html","ebf4554fc1cf4cd65f66078b9d179be9"],["/posts/2495386210.html","774b5bd1cdb30247114056e152809d4c"],["/posts/2516528882.html","05d951e2e0c7dbe7adadb30dd6c37d55"],["/posts/2522177458.html","65654e911a1f3030d43a0c02ffd4aaea"],["/posts/2526659543.html","90856a648649b597b8bd84a98386810a"],["/posts/2529807823.html","7f22b062487ed6a0adc6766f0a0c01f9"],["/posts/2596601004.html","9ca1f0bee1e7c7cc4d16948b91a1da05"],["/posts/2697614349.html","e28f24c02ad13499fe7bcb675e3882d6"],["/posts/2742438348.html","4446abe129e85f5c784269c2d0ca8372"],["/posts/2768249503.html","06d407a8f0685b8de92fda9deee16283"],["/posts/2864584994.html","4e698b8e627b37dcac158d7b0927c0e8"],["/posts/2888309600.html","6d38de93759c36c3f367ecf62dbf766e"],["/posts/2891591958.html","ec1bf22de63132ca4c746f83cef44892"],["/posts/2909934084.html","206a426dab56c93aed56841e21f574fa"],["/posts/2920256992.html","453bba7f72817d1611750ef1c61b841f"],["/posts/2959474469.html","a5b89ed1091586978213bc06dc68dd78"],["/posts/3005926051.html","bbe8520789e6f1877bd2a1c27034d01b"],["/posts/309775400.html","aa8498597becf6894ab94ab97f5ba67b"],["/posts/3156194925.html","688371644666ebd9534fe91f114c158f"],["/posts/3169224211.html","e76c3480d8b670c53ad6f33e15a65327"],["/posts/3213899550.html","8a61b3a086734ae549c459264cbbf7ca"],["/posts/3259212833.html","11607607adfbf648e5d6197ab4020acb"],["/posts/3265658309.html","ebcdd6966e83143fe077513e64ae03aa"],["/posts/3266130344.html","97d22b3749e6093c44e6a48683c07b8f"],["/posts/3292663995.html","0796ea57ced47a359723e5378489aad2"],["/posts/3297135020.html","4d10a8cc0d0989b0bc6182ec12924e0f"],["/posts/3306641566.html","f1eefe8f6473427ad46452b3ebbaa232"],["/posts/3312011324.html","73a5374f9fd4f0163d8d59485f13b799"],["/posts/336911618.html","5f3b2844c9720f350fbc924288e948cc"],["/posts/3402121571.html","2bc847b640c497cfdc5d7bb52cf82fe9"],["/posts/3405577485.html","b364de5b53dcb397303614b4a4f11015"],["/posts/3498516849.html","18f37dfc798a5d79cdf40df55aaf33db"],["/posts/350679531.html","b0e1c721b1410da236113f529436a3a2"],["/posts/3513711414.html","9b549fc261f1c4c3df63a0c0b737fccd"],["/posts/3523095624.html","e02a73556402f3cd9348013fe8a03464"],["/posts/3546711884.html","9720a76638e4f1941adde1bcf9537cde"],["/posts/362397694.html","e5d0a797bd0ab9cc60f6a4899979f2c3"],["/posts/3731385230.html","4d9b3c8c19135343e7c0fa19aeba02eb"],["/posts/3772089482.html","46843c2e003ee9195850773ba5105595"],["/posts/386609427.html","dce78125e156d3de0308d091632c754c"],["/posts/4044235327.html","43aa17401937a238c65638d75e7b28c5"],["/posts/4115971639.html","55f5bfd3cc5af20f1e8df6b8ae7f59a6"],["/posts/4130790367.html","0f2292985aae1b6b5a38ec7af6415c94"],["/posts/4131986683.html","264c5a142c2a7a35f74b7f10f03aff60"],["/posts/4177218757.html","3600692fc4d4e0974ec63b5d8ac618b8"],["/posts/4192183953.html","4e7f63bd4478d0a5ce4d37a173cccce5"],["/posts/4223662913.html","93a2d039e8bcd50bd600ebe75f849b71"],["/posts/4261103898.html","f2cc9183504141d96290feecc4f4de5b"],["/posts/4286605504.html","a8d019e77e1a861f3ef365fcb4c175f6"],["/posts/449089913.html","16cbac57ccd736a9a808255655196834"],["/posts/469711973.html","2d4bad3f483a5707d5ced2849a9805f7"],["/posts/482495853.html","e768415a7d8ad33be87574e189f26153"],["/posts/488247922.html","8d7540046df0da3b8b58344c6da105c7"],["/posts/517302816.html","b1cf0042b83ed9e092b970a1985ea3f8"],["/posts/570165348.html","042ccaf511bc0f2e1a000e3162759013"],["/posts/595890772.html","9861d67db5f1639fe8f8c4f53a0b6b07"],["/posts/67485572.html","e2c8e9e2ac806784291bbbdbfcf95dd4"],["/posts/694347442.html","02892f256f91c41960dc7185275014a2"],["/posts/707384687.html","3545bb46d1a3ed722e851a02a26a2e55"],["/posts/71180092.html","1056186b7c7a7df88e7c512a31ab0441"],["/posts/716459272.html","c9a47a69148dc4760cbf1230f8afbb3f"],["/posts/765481613.html","0c26f6f976fab7b16a2db29516d4d97b"],["/posts/778231993.html","4a6e6bed0f775d07aad2fbed94b93349"],["/posts/795397410.html","8a7acd52e55d46ad9db682ba758e03bd"],["/posts/820223701.html","32d340c92f3b5f4f28c877308ddb47ce"],["/posts/830372185.html","7943b2e4a5610a08a377f984076a05e9"],["/posts/88294277.html","8520fd19005ac5f13e78cb8e1cb65fef"],["/posts/939963535.html","26d48bfcf3096f88a374998bfdabf694"],["/posts/983786067.html","370e13ae874381ddc7734f6e953c6379"],["/sw-register.js","82cfdeec1d6c708608f1a69c98578b11"],["/tags/C/index.html","18be2a0cd6798a807db01960c081cb51"],["/tags/C/page/2/index.html","c02bffc22146ba4dc584a369af8f1e16"],["/tags/C/page/3/index.html","5ab4014b9bd93c3924d0383cd26175c4"],["/tags/C/page/4/index.html","a7492f64f2e432d7b1c01d4c29a6d26d"],["/tags/ETL/index.html","6c5e8fe51b5d7c9613559fba6eb91dbe"],["/tags/ElasticSearch/index.html","5172f17820dec5d10172c09c5df13fe6"],["/tags/GUI/index.html","273f7cb8942ebe6361ab6d9d98dd3912"],["/tags/HBase/index.html","c6199d2e01940fc501211e1aa723ec1d"],["/tags/Hadoop/index.html","a170422162e40e0011d8083c742620b8"],["/tags/Hadoop/page/2/index.html","36c4707bf78b2276d252d2b7e3696483"],["/tags/Java/index.html","f284f505198c94886926a83b700bb3dc"],["/tags/Java后端/index.html","3a13400b8ed1a63073338abe1ad03551"],["/tags/Java后端/page/2/index.html","292646882d736d74b7fecc122016c6cb"],["/tags/Java基础/index.html","ebf82378a3e4bdeecc2cbef72fe479d7"],["/tags/Java基础/page/2/index.html","2dbf7c6d924e33cd2ad2422d40290ab5"],["/tags/Kettle/index.html","3aba328abb9989060b702d220bfc906c"],["/tags/Kibana/index.html","1bdd8f814360e344b265b5e52c118cfd"],["/tags/Linux/index.html","fa13598bff16fd2d8b6235554892e45e"],["/tags/Linux/page/2/index.html","50c703b70423960a1813008e44078a4e"],["/tags/Linux/page/3/index.html","fabcf1aba2cf79b9f41976030e01ae77"],["/tags/Mac/index.html","e172d7277db8f2a43b000b5ffa06fc0c"],["/tags/Mac/page/2/index.html","d2d387ef128317ad2b5663ce8fb4d60b"],["/tags/Maven/index.html","7e5adaae53da87844b4237f73776d3fb"],["/tags/MySQL/index.html","41dad82aab994cb09039bce138f3fdf2"],["/tags/Python/index.html","a44bb70f8551a269d63671c46e2a983c"],["/tags/Redis/index.html","cf95c3911da0e189bd0bcda4711ebf8d"],["/tags/R语言/index.html","485268e53477e23c6a56b28dcc5c598f"],["/tags/Spark/index.html","c70759d228aa64315ef06066396b5023"],["/tags/Ubuntu/index.html","3f9c2ac8b337f8d5d11bc814224604d2"],["/tags/Vue/index.html","3239787b6015cd1e804196845ccf6f85"],["/tags/Windows/index.html","45dde6ea69a4d9014556311646a643ac"],["/tags/ZooKeeper/index.html","2b3cb731e4f35cf538affcd69ad0d15a"],["/tags/bfs/index.html","929201879b094873366a2ee081a5cf7e"],["/tags/dfs/index.html","277c5f33ca18284ba68c54c9a8b49bce"],["/tags/folium/index.html","fb555939267008c57e68131f5c9ea729"],["/tags/git/index.html","a7535ba35443194d00776605cc787d69"],["/tags/iPad找电子书/index.html","60c9d188e91d449c0d5e6b69091197fd"],["/tags/index.html","93044cc8c56902dbfcc9c4322259cfe5"],["/tags/latex/index.html","17d3a79ca75c93104073084b4a7b1bab"],["/tags/中间件/index.html","4dd3b7c58967423babafa944c04e7fee"],["/tags/二分查找/index.html","ebde73a9f0f872a6e4905220d5da5509"],["/tags/优化类/index.html","95154a47e59866e503daab9ae1403417"],["/tags/前端/index.html","9808553fd57e47e309db122c49696e84"],["/tags/前缀和与差分/index.html","a2ebf75e2052c77cac6b39844eba5983"],["/tags/动态规划/index.html","66956817c364c72ded1f974d2cd2bb7c"],["/tags/动态规划/page/2/index.html","d60dcf59c1397904a6ad3b6f47906b4f"],["/tags/博客搭建/index.html","73d26261a03494ae473db2499b984413"],["/tags/图论/index.html","b3f4be30c6fee1966d7c67e8563c359e"],["/tags/大数据/index.html","d41a81bd129123386b40ec6b3a889e15"],["/tags/大数据/page/2/index.html","cec51643bb82d837c6d31f89f6f740a1"],["/tags/排序/index.html","3b0f09520b005d15a9d7981d4fd2e605"],["/tags/操作系统/index.html","37f33251d7c37203a8deec6c3536a55e"],["/tags/数学建模/index.html","ec60442b162b8e9bd6193c39d763969c"],["/tags/数据库/index.html","2a4444fcc142951c2f969efbdb96b96f"],["/tags/数据结构和算法/index.html","85df8969ff1fdf0113542ea1d1f27363"],["/tags/数据结构和算法/page/2/index.html","d560d9108e91583ce7603bf2e524ff32"],["/tags/数据结构和算法/page/3/index.html","cecd419359a50d80f05d64d4564ff33c"],["/tags/数据结构和算法/page/4/index.html","ea9de40da9b972ae3741918ab46b72b1"],["/tags/数据结构和算法/page/5/index.html","2ecba9ba60e86730a0721d43bcf367b1"],["/tags/数组和字符串/index.html","8be3d59a6b5eb504254eb4ebbb1991da"],["/tags/数论/index.html","acbc6ec2f0ffb6461b73b362a0a55162"],["/tags/枚举类/index.html","339dad3d17e9d947fea288f8bb0aa087"],["/tags/栈和队列/index.html","e992fa08229f6cc5aa6781a7a3ebb982"],["/tags/树论/index.html","79cc532f10d71d83b7c37050885cce70"],["/tags/测试/index.html","039e64a2a2f92809f401d57ec38e9159"],["/tags/环境/index.html","6667fad486cce03d62fa579ad6b4efee"],["/tags/环境变量/index.html","668edb8c56d9dbf5f06701524db92c8b"],["/tags/绘图/index.html","332cfc072fec7e9263949baff9f6ad21"],["/tags/编程工具/index.html","59e3c5025e5e7c6e4df270f259951cf7"],["/tags/编程环境/index.html","cc0892633cdd1e7b5c68b2117266b95f"],["/tags/网络编程/index.html","2a1beb8f3058c558ba519768b3b27e67"],["/tags/英语语法/index.html","783924d308454db06ef93f94504df866"],["/tags/计算机操作系统/index.html","993fdfbd9c7058f7b7dee4e2671a3269"],["/tags/论文/index.html","ed4eb9abe99f2be9c95459c3a54a7eaf"],["/tags/资源下载/index.html","6b66b9b2c2adc1419f99c894ec4056f2"],["/tags/链表/index.html","2b047e930b59b73035f10b0f3ef42de6"],["/tags/集合/index.html","dea54abcddf90b525f9061d79c95c84f"],["/tags/集群/index.html","9ebe2e7556cc8a922e081b2708edc813"]];
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
