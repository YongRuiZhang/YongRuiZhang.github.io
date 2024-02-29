/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","5762c1ebc323a753b1ffe4cf16d50940"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","383bab2c27d3ab63cf35f62f92d6587c"],["/archives/2023/02/index.html","63d33195f8491e0552a41f0fb0d1040d"],["/archives/2023/02/page/2/index.html","f169ab31df345f7a45dcce8412a83c22"],["/archives/2023/02/page/3/index.html","905756028d3a6c6c73118a9c96ce6d5f"],["/archives/2023/03/index.html","80add393cfbd50e2ea07b9100f42f0d7"],["/archives/2023/05/index.html","cf3d16e131ed983c2c94b61453ba7f14"],["/archives/2023/06/index.html","45d5f7a2a1438a6c86ee0551c542229d"],["/archives/2023/09/index.html","6cfeca9c480fe3aa3e2f83a6a9061a5a"],["/archives/2023/11/index.html","5a3e4e41458016599ab08640926b60f8"],["/archives/2023/12/index.html","04d0785c334734d759b5ba62f3aaf6f2"],["/archives/2023/index.html","026081c8cf37eedf363efa04ddaf880d"],["/archives/2023/page/2/index.html","3cda06007c42abfa8b86b20795a2f214"],["/archives/2023/page/3/index.html","c99f31c3cbf41ec53d40d4d21bd5403d"],["/archives/2023/page/4/index.html","f131077d833057c3e15462e3edc685cc"],["/archives/2023/page/5/index.html","1a3c85e4ca85055175c44ee86ada5724"],["/archives/2024/02/index.html","d10edbad64efc3f184887fec5edbe080"],["/archives/2024/index.html","c39ef86f1547899ba86afc27d76514ca"],["/archives/index.html","b627f0209342558be1ab887f49d3dff4"],["/archives/page/2/index.html","18295ca2ebfac4bc92ccc4bd334f05b9"],["/archives/page/3/index.html","4e371e494109f09827a2d8e03d409eff"],["/archives/page/4/index.html","82a9aab4850f66f57cad8615eb39485f"],["/archives/page/5/index.html","8a0d10f8e0e80407a7e2cc6ad3acf310"],["/baidu_verify_codeva-qQP2iZOMLX.html","1a4a4afe3937839cb913a0b95e9c1521"],["/categories/Java/index.html","61781521c2fc28f5512c5689a5ddb121"],["/categories/Java/后端/index.html","a11f151581aa850c5b4aef8ef77a3251"],["/categories/Java/基础/index.html","0d231603e0d504f435b5cd8423e1b99d"],["/categories/Java/基础/集合/index.html","c451eade2bf4c26d0fd7c3045b3b1cb6"],["/categories/Python/index.html","10208b3aaeb831b364784de4680477f3"],["/categories/Python/编程环境/index.html","770fbde38bab01cfebb190f7030d8b61"],["/categories/R语言/index.html","aad08a4818bf71826bc6f6ed4a23fe14"],["/categories/R语言/编程环境/index.html","fb8f7c70159a6dc1c2df99aa002921b0"],["/categories/iPad/index.html","3504c6a1b2a3b815c9e1aa10def261c3"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","caa6491d32b1bdc773ab8f697351d6fc"],["/categories/前端/Vue/index.html","6aa1f27f63df843fb03e1335860ff8d6"],["/categories/前端/index.html","09cf83bd59e8cc36bb3f68a510412184"],["/categories/大数据开发/ElasticSearch/index.html","d241ef0bc46a7044f69386ffaabd5cdf"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2715209db715494a7b020fb7aea28296"],["/categories/大数据开发/HBase/index.html","a628740b0c6eef967ad5a419080e4e5f"],["/categories/大数据开发/HBase/学习笔记/index.html","28f17bc073fb0673289005cbe390926c"],["/categories/大数据开发/HBase/环境搭建/index.html","f4e73334aeda82ddaaa9a805b7fd4343"],["/categories/大数据开发/Hadoop/index.html","542818881e93939a01c6fba40d6b0fe9"],["/categories/大数据开发/Hadoop/技术/index.html","d837a2edbea87fc6fe1470642009a094"],["/categories/大数据开发/Hadoop/环境搭建/index.html","983ddd2232c56d8852ce746aaf0b5985"],["/categories/大数据开发/Redis/index.html","e029554081f5264419790823975b1da8"],["/categories/大数据开发/Redis/技术/index.html","4d0d5f644d36b219d9fe96bd294c69a6"],["/categories/大数据开发/Redis/环境搭建/index.html","38fff5f8daab9fca6319902f83671618"],["/categories/大数据开发/Spark/index.html","cf5c561b1fcee35ac43627b125b8027a"],["/categories/大数据开发/Spark/环境搭建/index.html","a5bdc6bbeaa10aadfcddba802828ec79"],["/categories/大数据开发/Zookeeper/index.html","29d229e34bc874eef8001d531fdd41a8"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","328bf33e3ae74e5a165b49f272c02ed8"],["/categories/大数据开发/index.html","a6934159edd0a9f91cfbb03afd83bee0"],["/categories/学校课程/index.html","ec6740be79fca7f831663bc9c390218d"],["/categories/学校课程/计算机操作系统/index.html","b92318a2dbf0e38fd38efd3e9e0620f1"],["/categories/操作系统/Linux/index.html","f4ed16830541eface3a14dd2ecdec665"],["/categories/操作系统/Mac/index.html","4eae284298a87b4984fad9fee785af63"],["/categories/操作系统/Windows/index.html","0d3af27a3297c878cd0abd5a16dfe2d1"],["/categories/操作系统/index.html","04c193de5853c4e4511db8a8e64a836c"],["/categories/数学建模/index.html","6f0d53555ef84691422f8b290a4de795"],["/categories/数学建模/latex/index.html","63f92c7ba8e851443922ab5927d9029b"],["/categories/数学建模/优化类/index.html","1d96dd8230da6a36a1b82200f3ed00bf"],["/categories/数学建模/优化类/现代优化算法/index.html","4d87b36da3577cd8e6b2c215c2f2e079"],["/categories/数学建模/优化类/规划类/index.html","8a56a52f6fd2e831c2e526337c7e79f9"],["/categories/数学建模/绘图/index.html","eaaea97f85f994fff1687af66d0539ca"],["/categories/数据库/MySQL/index.html","f2a6e8d19114116c954ab7caa5caa78b"],["/categories/数据库/index.html","0d3e4dfc4ef85524516bfa7e3985d713"],["/categories/数据结构和算法/index.html","5fd836148f17c1b3fa5fed20d55b254f"],["/categories/数据结构和算法/page/2/index.html","f7051bce258a2abd3768788feec5f3f9"],["/categories/数据结构和算法/基本原理/bfs/index.html","9a8e1b4b6efd8ba570fdb377e1645910"],["/categories/数据结构和算法/基本原理/dfs/index.html","f9523fa062f8c700b9bcda97a9dbd376"],["/categories/数据结构和算法/基本原理/index.html","4d5b113f26e9f9982f6627a4f4a9a219"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f1ef87a147b764ae5cf77fe319453d56"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3f53144739141360d27e3fe618fc877f"],["/categories/数据结构和算法/基本原理/图论/index.html","cc59109e6aa9917a1715240b4af82e6a"],["/categories/数据结构和算法/基本原理/字符串/index.html","dbe6610c47b185bcea29b8b2be4b4df5"],["/categories/数据结构和算法/基本原理/排序/index.html","9febf445491d86f59adf492bde36da82"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","090d4842d6c49f33d93e9b42920e518c"],["/categories/数据结构和算法/基本原理/数论/index.html","bebbcd2bf79b468ed3ad73d9767f7eab"],["/categories/数据结构和算法/基本原理/树论/index.html","adcf694ba4358f8ad68fe4b639ac2cf1"],["/categories/数据结构和算法/基本原理/链表/index.html","e4372b6302d7a4dec31778f5c9413061"],["/categories/数据结构和算法/算法题/index.html","16782cd9b69aa5aec501043bcd9d27a8"],["/categories/数据结构和算法/算法题/二分查找/index.html","6aac7982eb2abc5160d3df498e806311"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","5403a7c2e7bbbaca57490049506f3cd9"],["/categories/数据结构和算法/算法题/动态规划/index.html","267909c463d19ec544f1a27c12489c73"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","db1d1ac6138dcc3239da6e1461674b84"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","12856a30b1c0581734ccbc2dd728dad2"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","c50e7fdf206deee3ff343f74cfba3d01"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","19e839e05d48377fe049873c367bbced"],["/categories/数据结构和算法/算法题/数论/index.html","8fd4acca838292e571122bb8f8dc76c3"],["/categories/数据结构和算法/算法题/栈和队列/index.html","b5e68eb88c4e0b60f74a45b7a9e5fa96"],["/categories/数据结构和算法/算法题/树论/index.html","a7351fec695b44d54243125439d318b4"],["/categories/杂七杂八/index.html","34787fd4336c54dd9630a850c3286bed"],["/categories/杂七杂八/博客搭建/index.html","3a1550f72ee768b714be63d4e465a25f"],["/categories/编程工具下载/index.html","1bfa3718cf570984f2abeb30d13ceaae"],["/categories/编程环境/index.html","39e12e4448bd0eecb327c9c0838e2943"],["/categories/编程环境/大数据/index.html","834b3283483fc0064d88b0f0fac7618d"],["/categories/英语学习/index.html","9c140cdb8bbcb37dec3844c9b1646489"],["/categories/英语学习/英语语法/index.html","8a48c260fa9575bd871a9b375faab256"],["/comments/index.html","04b54e7be6c1354563ff4adce84fb66e"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","577614258e5a976e4845fa84f9df3297"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","51e1eac6e9e3b5d4df729cc8181ff070"],["/movies/index.html","76cb7267ca428eaeed3e3a1eaeb7fe46"],["/music/index.html","84aa971f301ffd3181ec6c4d288abfe3"],["/page/2/index.html","e50af2a7ffec45b99c9ac1453f93cb5b"],["/page/3/index.html","4a6e1a02deb2deca2d8e21104461618a"],["/page/4/index.html","87bdf2f8cb346683c554fad79075a43c"],["/page/5/index.html","812aab65dfc5993a1f48403b7ade00c7"],["/page/6/index.html","ecc32942cbeec3869d805d43c9a96378"],["/page/7/index.html","5cc0522902d873e66348b40cd5f99124"],["/posts/1021360842.html","9d8868de414b3c0a152d0e34684886b3"],["/posts/1120620192.html","28ab79c982ab596c0076ac8b04e23d7e"],["/posts/1137707673.html","b93135a40ef322222bab33952f5df4b5"],["/posts/1141628095.html","0df789333c078ef2b8a3e615c1d718fe"],["/posts/1168613674.html","396ca8b06309dc3537ff9665f2272037"],["/posts/1219920510.html","481f3d62d4205a06b8877eec9d99b241"],["/posts/1222166338.html","b6837f7b0b03616769e6fcb5a99fb6b9"],["/posts/1259097482.html","f7c26be7d929ef78bf4bf08daf2dae17"],["/posts/1271036369.html","25d79d6391d566c64ed91cda2f380a13"],["/posts/1312847445.html","7f5adf01efcef4936de59b884ef855df"],["/posts/135355774.html","dc8f682d39f205b32934a469ba6a1c27"],["/posts/1375344716.html","e0a804c9f3413823f4fa8ffc3592ae50"],["/posts/1388991698.html","11f70c3445f6cf37ce8cf3120ce52978"],["/posts/1410315814.html","66bf11d90ed9a63f77ffdb0b2f6ff4c6"],["/posts/1452790229.html","4bbee1a2e14ef58de355b59a04b6e28b"],["/posts/1470079884.html","78989472aa68cc23c92391c783ac54d8"],["/posts/1470079885.html","6fcbb5e9e55b76d0859fe28c07c96633"],["/posts/1470079886.html","f64a04de624d588c1de096486209c1e1"],["/posts/1470079887.html","94f00f7aaffb84e3e81bdcec129eb4af"],["/posts/1498536549.html","9c31b01f7f3cc7b0bae78e7e09abaa99"],["/posts/1539568593.html","604a16cef6c51c0fda88e9d23d221bdc"],["/posts/1547067935.html","2acaa2b7409d947ffad3c151e245ea9b"],["/posts/1557866301.html","c02dc1b3cd1faa2ce244ade667ccee50"],["/posts/1571776361.html","4a8279cf30dacee5e546fb3c36be324a"],["/posts/1605124548.html","f41e22df8083465b0b046d0d053eb810"],["/posts/1633036852.html","6b12ebbbb8d4a6c2938ce95ac52ce23f"],["/posts/1667740714.html","457ab57d10057fd932f2e8537f781bc8"],["/posts/1674202625.html","d78c66b509b9a1642ed9fb992d3a7e91"],["/posts/1765123828.html","ad37f126dc2fcae8ce27c2204be9121c"],["/posts/1767336200.html","ee2dbfc0a66a2a92a2391d5df79b04b2"],["/posts/1776114197.html","0185a3c6ea2d23d39d7f4f0e101731a9"],["/posts/1817748743.html","ca9ee988054bcfb4375d49652e0d4c47"],["/posts/1925125395.html","559cc1eb4b5a20bb46295c8fee05c560"],["/posts/1966191251.html","4866127ea48e6d854180daff8dfbdc88"],["/posts/1987617322.html","49e1c5ba4f240f5b6e121c3905718c38"],["/posts/1999788039.html","0ab9897ae4e011de79dfa9bdf8b91915"],["/posts/2075104059.html","6cda925700651aa2a58264d295d49bae"],["/posts/2087796737.html","6c902eb8c16c184487573a025efda59e"],["/posts/2106547339.html","5c203cc7bb29e473b10d1201a3553a3d"],["/posts/2207806286.html","62cd1be65ecce3393ad2214078ce7e3c"],["/posts/2225903441.html","63e42fff17ca618a4580f0cdfd29f318"],["/posts/2265610284.html","56349aa5ed59ee960e4ddc0aef6b0a02"],["/posts/2281352001.html","73da6dd78a19b40a5eb84cb6297f73bd"],["/posts/2364755265.html","bb342f0ce7762ca0e5eaf49640e5a73b"],["/posts/2414116852.html","83a9dae50513411e6d22bf3861c2eab1"],["/posts/2421785022.html","f334ab22799a4ddb9ad1ab8f058fadef"],["/posts/2482902029.html","e50eef40dc51ba4dd561dc9cd98bca40"],["/posts/2495386210.html","3dcf0c1b3be6dc6aac53350168851e6d"],["/posts/2516528882.html","6a73d065c4bd7c703ab6ecdfa43fd807"],["/posts/2522177458.html","0ee3f91f7cc30de2d567d3ab0a927dc9"],["/posts/2526659543.html","21c23ca1c570711211e1f3c0b39d8bb9"],["/posts/2529807823.html","c35f465f39160e63abc16078c4a6a26e"],["/posts/2596601004.html","0882094802ffb2e10dd86970c995e1c9"],["/posts/2697614349.html","88ff5682f4b2d3b26e03867d2db3a551"],["/posts/2742438348.html","cc67bd6973b8bbaa52aa3bbf2c583732"],["/posts/2768249503.html","0930dc00f7c094f0144fd43c2d4849ad"],["/posts/2864584994.html","b5ed2e38fa06ea433ff5c49d9adb98b1"],["/posts/2888309600.html","cd710e9ba7f958d570996692d69e2746"],["/posts/2891591958.html","ba06af6a003f511551c4d0ddf2770715"],["/posts/2909934084.html","7d7d342c9b22e9e50cd23ecf9a3b0c28"],["/posts/2920256992.html","1d977566195558ca8364a93b45269234"],["/posts/2959474469.html","a6dc717587ff5c783545f0ad2115fa90"],["/posts/3005926051.html","eb334a66db1cfda00cd7b93556e64e8f"],["/posts/309775400.html","2efaf915c34e2b57e1844678d3223461"],["/posts/3156194925.html","2bf2162102575fb0ca77836d78702e16"],["/posts/3169224211.html","f24537c491de424c9d3c38d7d9a70973"],["/posts/3213899550.html","2380691d5dc6da1d72da933c31fe0e21"],["/posts/3259212833.html","b52c816e896100a0808d9ddbabf7d6b4"],["/posts/3265658309.html","9c1a0cc1c96c75154cfd49ff5f05fca8"],["/posts/3266130344.html","3bb748469be2a244e15034948c87a746"],["/posts/3292663995.html","a3faa99847fcee49c688017b7e0c2d06"],["/posts/3297135020.html","a6dff95bb80dd237504829db88eba801"],["/posts/3306641566.html","4bcbf9a1158d6682e4031085ba02d99e"],["/posts/3312011324.html","3d48fbfdb59d3b8653bc1165ba7c44b6"],["/posts/336911618.html","3a8b64ce4c7597ab2b3d300b066f0d4b"],["/posts/3402121571.html","52eee830d4e476ca971803ccb0055f09"],["/posts/3405577485.html","ac218ce8673c65428320de4161e4439e"],["/posts/3498516849.html","0950123acf10ef79c0cb2ac8e4fdd572"],["/posts/350679531.html","c8399193b213b33d06b0f5d973232199"],["/posts/3513711414.html","9b195b7dcf2c7906483597956ed75476"],["/posts/3523095624.html","2900a99c37dbc234fa5fc69dedab5253"],["/posts/3546711884.html","8884e18da3047569ef12832db32c06b3"],["/posts/362397694.html","62dce4eb04ae5235e3339336fba3030d"],["/posts/3731385230.html","a3fe5ce73d55ab7ded79484c98ade1cd"],["/posts/3772089482.html","53a124c8c64d1807c034e17fbf61566f"],["/posts/386609427.html","bd003695e6c074d003d7497ba07e1928"],["/posts/4044235327.html","91469ead121aae15ef00be03e5ff80d0"],["/posts/4115971639.html","3c9c3d202adee1c50d3a542d834f62e7"],["/posts/4130790367.html","d9746bd2dd34929236b68f8fbcf7622d"],["/posts/4131986683.html","9c27bb384862caa48b577b33c93ce01b"],["/posts/4177218757.html","b09e572f65f93459c0764a0bb3a7eeb3"],["/posts/4192183953.html","e67502c78ead2176c168580ead2de7b3"],["/posts/4223662913.html","54af47f071cbd615edcd87af5d0b4dc5"],["/posts/4261103898.html","b71a8bb91c9c0dae92dc31196b882694"],["/posts/4286605504.html","106e305e770831ab6bec0d3ee9ced6e6"],["/posts/449089913.html","067ba9923684f08669d32bed3ffc8fb8"],["/posts/469711973.html","dcdda31f0ba596e1505a0f12c04151ee"],["/posts/482495853.html","8f582dfc85d3934647ac183fb20063e3"],["/posts/488247922.html","121d9c965116e982ff64d1440f035137"],["/posts/517302816.html","11a9031bdefcb4cef5aedbb71b86b64e"],["/posts/570165348.html","cb97731483b71ca7bde87ebdeac58be6"],["/posts/595890772.html","30abe322dbd6cabe3e59d3f8c08cd6ea"],["/posts/67485572.html","13ca1e51a1dafa9317dd35354d31ca14"],["/posts/694347442.html","d9f0af37bcfad244648540987c7150b3"],["/posts/707384687.html","482dc95ea3d8505be2b435351444d661"],["/posts/71180092.html","afff022d45ad58e5883bf7db13d5c2c0"],["/posts/716459272.html","105f2936c9198ff14dcbc99e6f0daad0"],["/posts/765481613.html","a42f6499b3dfcd901a29b35008ab79bc"],["/posts/778231993.html","a341f83c7fd696a6603b52090a6ed5f3"],["/posts/795397410.html","e2154bdf3b89b0dad0f3fd0321d3dd71"],["/posts/820223701.html","2dd2941c8d81c70ace0eeeb7f64da91b"],["/posts/830372185.html","0924222c7c9cefbc4aad3fd1e9cac757"],["/posts/88294277.html","e105b8f0a2bc1421ee1beec7e0600bef"],["/posts/939963535.html","3f41168ae555fae32ba5eaa35a87d6a3"],["/posts/983786067.html","06511762cddcf843155535f53d942df1"],["/sw-register.js","b0e4cdf7b62b082cc6b51669090b47b7"],["/tags/C/index.html","d0c46bb67ac60deb622708746869f57d"],["/tags/C/page/2/index.html","0abd049c3d2c18b0113377d7cfb72583"],["/tags/C/page/3/index.html","98479b4d1ba3b06b9eb5374f8e96f00e"],["/tags/C/page/4/index.html","5e03362d772547e9881a3a84e201a2d5"],["/tags/ETL/index.html","84931047e1ea9ccfc11365c421e5ccc6"],["/tags/ElasticSearch/index.html","c57ef9e16a59849e67ea9318187a45e4"],["/tags/GUI/index.html","104a7760178b600cc368d516e08c086a"],["/tags/HBase/index.html","c9f348147ded843a9e2c73191985db37"],["/tags/Hadoop/index.html","05bb89758728c5ca875f4d949e2e0f5b"],["/tags/Hadoop/page/2/index.html","440c000330aad92fb5ee7b55d13ded20"],["/tags/Java/index.html","d990b3d775a7ea686cc0335876a4362b"],["/tags/Java后端/index.html","f819e1746c7b323efb019d812108c59a"],["/tags/Java后端/page/2/index.html","dcf793c239299c1c4abada2aa0afd14a"],["/tags/Java基础/index.html","cec0d95ec7f345bcc80998d3bb14f706"],["/tags/Java基础/page/2/index.html","687d8de3c80fbcf5ab29289febf17b47"],["/tags/Kettle/index.html","b4e40f080a378fc2ad24fbd7481a619b"],["/tags/Kibana/index.html","6c7ad3621b7547c010b87addc7a3e0e4"],["/tags/Linux/index.html","4fcc384fdaa3e4b8232d1bafb4e37a4d"],["/tags/Linux/page/2/index.html","690496e988ccb9796480ee7dde190078"],["/tags/Linux/page/3/index.html","63d90c70cb6f95ea7f36e597820cfef4"],["/tags/Mac/index.html","c118cdb39f8f549b98ba53dfa5ba001b"],["/tags/Mac/page/2/index.html","32a312c1d60c62521c33ffcaa074632d"],["/tags/Maven/index.html","bea7fb2cf0805eab4daf5eb8432cf4ca"],["/tags/MySQL/index.html","5679675e5af35eb9922d944b5b6b20cd"],["/tags/Python/index.html","a8a109b546e1e173368c3528e2bb5686"],["/tags/Redis/index.html","13a538617b1d4c053a5384c27a335563"],["/tags/R语言/index.html","28b5867099dff621c79f855904df35b5"],["/tags/Spark/index.html","4fa9c57d2eee11be967561375079b402"],["/tags/Ubuntu/index.html","1b473b95cd8dbe80dc94646f8761d64e"],["/tags/Vue/index.html","fb444e1cebcf986c255b341376039e79"],["/tags/Windows/index.html","edf2382ba86cb404dd49588c30a8a94f"],["/tags/ZooKeeper/index.html","2fae0e272274af4817c49c73c03d7080"],["/tags/bfs/index.html","1d1489d7edb28a8dd13e93ffe95a1245"],["/tags/dfs/index.html","92fd9718d8a0a2a5e2dd9b01f2df9ae8"],["/tags/folium/index.html","1671b0646b6bdbdf5386d0e3b67b1808"],["/tags/git/index.html","b5ac4def50cb530fbd795366990ab3ca"],["/tags/iPad找电子书/index.html","56cf12e97e0854b15e2c26e19e7ccfff"],["/tags/index.html","c610f56d8cb2d529f991a8483a96f886"],["/tags/latex/index.html","3b8a22c271bce1e86e0900641e993519"],["/tags/中间件/index.html","d48ff7eadf54171c19fda706226d27f6"],["/tags/二分查找/index.html","9968b617d4edbcdf14323c6a5af4a47b"],["/tags/优化类/index.html","d45e77b60867435338fea61e7ba89b0c"],["/tags/前端/index.html","ca31d56e1505adba16c1b2134ab3e810"],["/tags/前缀和与差分/index.html","04c658ee0899c234b4074bedb92ebabb"],["/tags/动态规划/index.html","c0bfdc0cfa25773826c3faa42a1140c5"],["/tags/动态规划/page/2/index.html","a25ade2fa390ffc7a02c4f39eed55107"],["/tags/博客搭建/index.html","4ef4a372d1ef7a327c4bee1dfc5a2c8f"],["/tags/图论/index.html","ca826598b8080022b0722f29c74ca6c6"],["/tags/大数据/index.html","b4e7614cdd2120220bae845342ca7e60"],["/tags/大数据/page/2/index.html","ea8c48c7d9ad93074ed4489b21f422e6"],["/tags/排序/index.html","f048d1fa610ddabc3478049300442287"],["/tags/操作系统/index.html","1422be2c82df02757e9064970874f0e7"],["/tags/数学建模/index.html","92429a0911a103ac1c6896112373cf99"],["/tags/数据库/index.html","1ac19262e40e38b1912e079a4dcaf208"],["/tags/数据结构和算法/index.html","47b4b2119559c470ab40a9690a4c921b"],["/tags/数据结构和算法/page/2/index.html","028b112d312ebbc6ececa8d9ac4d0b7f"],["/tags/数据结构和算法/page/3/index.html","86edb9cf1afbb0e15c799c96d00de9e6"],["/tags/数据结构和算法/page/4/index.html","074025f46464b55a5ab6398798ff27c8"],["/tags/数据结构和算法/page/5/index.html","de5916499bb398693b383c98fbbdb691"],["/tags/数组和字符串/index.html","ca335755972ee637e9d54b21066c5c63"],["/tags/数论/index.html","c07b51d85c28185a787440dcb3f8be95"],["/tags/枚举类/index.html","abaf9dbb176597472fbe6b461e49d009"],["/tags/栈和队列/index.html","2680e828bd0e0bbc8def946892199a20"],["/tags/树论/index.html","c78b0f0e1dc74c2b6ea43118bc1263f6"],["/tags/测试/index.html","c0f9a032ff1bb525bdb8afd9f89ad48e"],["/tags/环境/index.html","c974e9f4516fbd885b708d0455b2bfcb"],["/tags/环境变量/index.html","7bded5bc38f8ed4ab2fa490ab55b1e3e"],["/tags/绘图/index.html","1238b7779c6cde1b5903fdcdd5f2a9a6"],["/tags/编程工具/index.html","ac4421818dd14d24f22d6961ad64cd6e"],["/tags/编程环境/index.html","1697545201969fbf8f129ac100f2f7dd"],["/tags/网络编程/index.html","9045976d3d0c6dc04cfe925f94efb90f"],["/tags/英语语法/index.html","1e12347ec81cdfff45e1c3f770ba27db"],["/tags/计算机操作系统/index.html","24225ab7a9d82ce7dce6939d044c9760"],["/tags/论文/index.html","c82910cd2d5093cdf5fac112bb0c8e44"],["/tags/资源下载/index.html","ea41896d468f8148a67bc3e8cc75c731"],["/tags/链表/index.html","11ca3c6c9801df032c4fe7d0aa89bfcb"],["/tags/集合/index.html","a55a6f0ae462395d23cb24c50bc59cbe"],["/tags/集群/index.html","36a2482de915cc5a6db52865a820aecf"]];
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
