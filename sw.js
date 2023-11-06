/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","437debd1319f225cea0b08254fc53b68"],["/about/index.html","2fbe0e92beaeefdc59e6128a083f72b7"],["/archives/2023/01/index.html","2127f50c93a8bac05e2f7598afac6fcb"],["/archives/2023/02/index.html","69893e553b32161b8a652851699a4693"],["/archives/2023/02/page/2/index.html","d7fbada3ea46c5754fb668e06cef6ee8"],["/archives/2023/03/index.html","bdd7512c20247cf57cde91f68db289c2"],["/archives/2023/05/index.html","722401396291ce60de47bed5ced34806"],["/archives/2023/06/index.html","2c913b1d3580a2a651e2126b776c97d1"],["/archives/2023/09/index.html","e76fb9a4a11de8024cb48323da4925b3"],["/archives/2023/index.html","88dc5225f513ef2ccd7b4c8291117b06"],["/archives/2023/page/2/index.html","38a6c4a053e7e9ef538e42edc26aeb9e"],["/archives/2023/page/3/index.html","0859483bcc13168846b853fe5640e0f8"],["/archives/2023/page/4/index.html","fd9ebb41629c317c443df1f6c76d72fb"],["/archives/index.html","462a44d186fff4796e577689b0a25534"],["/archives/page/2/index.html","9b5cf202eb4b36e3cf471f86ce8a0631"],["/archives/page/3/index.html","3bc11ff0ac053682f35c6f9f88a603aa"],["/archives/page/4/index.html","0074c72f723f9c041cf36dc84663bbcb"],["/categories/Java/index.html","54de264a4ef1a9f743cc6295ef888a06"],["/categories/Java/后端/index.html","a46c959d34d8d6269b94cbcc1bd6bea0"],["/categories/Java/基础/index.html","2daa4430ad2915e7d89fc9e7ca898b4e"],["/categories/Java/基础/集合/index.html","49f12d726fe72eb7ede891e38a8e445d"],["/categories/Python/index.html","889e19bb7cad2e9712a89bda3805e6b5"],["/categories/Python/编程环境/index.html","4d42d70fd84446577eb87ea1bc0bd93d"],["/categories/R语言/index.html","c878da989d546b5ff1a3acc96c0c4708"],["/categories/R语言/编程环境/index.html","fa7ad3fbc99ad00b7f532fcb9dfdfcea"],["/categories/index.html","810ab95d55c71ad00e9987c0a37c9c3b"],["/categories/中间件/index.html","931ca7694617245651b983642618a068"],["/categories/前端/Vue/index.html","8bfb957bc3c79f1695e63546f3a79d35"],["/categories/前端/index.html","0eb383de6747d5034b2afc3f95a17485"],["/categories/大数据开发/ElasticSearch/index.html","516c280a0c2d8429e4ac58666582cc05"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","376995cc5eb32f98eaf965d461947124"],["/categories/大数据开发/HBase/index.html","016fe167afea4409ee1a49905e13b8aa"],["/categories/大数据开发/HBase/学习笔记/index.html","eb6a998b37e423e0fa21ca06b070cd84"],["/categories/大数据开发/HBase/环境搭建/index.html","7747925d235457b5da75c060a9ea6f7e"],["/categories/大数据开发/Hadoop/index.html","00cda9df23e2440de8ef9e340169d406"],["/categories/大数据开发/Hadoop/技术/index.html","2a09b0c716c17c9b138ff81077f79a1d"],["/categories/大数据开发/Hadoop/环境搭建/index.html","877fc90b6ef1b343878454689c7f674d"],["/categories/大数据开发/Redis/index.html","c952b46e9bbe86ba26da47989b1ee2f1"],["/categories/大数据开发/Redis/技术/index.html","f68016fa0b0b051dd7906d684008358f"],["/categories/大数据开发/Redis/环境搭建/index.html","0009c8aa23568846696ef30d843a45cb"],["/categories/大数据开发/Spark/index.html","af21b464391d551dcc80b004b9b2efcc"],["/categories/大数据开发/Spark/环境搭建/index.html","3d36ba816dab9119f7b003f2f7448d4d"],["/categories/大数据开发/Zookeeper/index.html","0b556bcdb2d0baa516953d88e7e7e60e"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","5b91a5c8b14e1a199c5e6242cf113fb2"],["/categories/大数据开发/index.html","8fe8457d9965b920026bd29d988bd1db"],["/categories/操作系统/Linux/index.html","ffcde21e8675d2a2aeb56345ce7445d5"],["/categories/操作系统/Mac/index.html","b6ab87327b94f7086dcb42e921cf8a27"],["/categories/操作系统/Windows/index.html","817ca2a7d514a94a8a9e8217776850c4"],["/categories/操作系统/index.html","e9dec3702efdf9c4cc288e45e82c9bca"],["/categories/数学建模/index.html","4763d5bae884812aeeb3f61cf8a408d0"],["/categories/数学建模/latex/index.html","f5281027c702c3224bd92fbfce14ee90"],["/categories/数学建模/优化类/index.html","80d2086a5f10c73510bced1fba000a23"],["/categories/数学建模/优化类/现代优化算法/index.html","459a4dbfbbffa7d3026629b6d12609fe"],["/categories/数学建模/优化类/规划类/index.html","ba1ac14005611cbf915c30f43007d3fd"],["/categories/数学建模/绘图/index.html","907130898c23a833680c347556bcd79e"],["/categories/数据库/MySQL/index.html","c2ebf71aed47ddd67725cd23b38b05cc"],["/categories/数据库/index.html","98ff51263f980710229de9f737fc8b70"],["/categories/数据结构和算法/index.html","6531e8b2adc4e243ef7662cf21f63ad5"],["/categories/数据结构和算法/page/2/index.html","e4fd9be76f9f6c729b1e09b56730a6ec"],["/categories/数据结构和算法/基本原理/bfs/index.html","af95a2e1498b4d8835fa7f222618d56a"],["/categories/数据结构和算法/基本原理/dfs/index.html","7c020156f90a12bc5a4d8efc876bff2a"],["/categories/数据结构和算法/基本原理/index.html","b2927fa06827c93042780b192ec87564"],["/categories/数据结构和算法/基本原理/动态规划/index.html","99f8a72688c7ed01bbecbf2ce0bb4eb0"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","bb602ee099d143e006b6cd7093512f78"],["/categories/数据结构和算法/基本原理/图论/index.html","912a9548209bcbf1b58d8f676a3318c3"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","a78db480e1ceeddb8b9c4bce87496de4"],["/categories/数据结构和算法/基本原理/数论/index.html","7a4178da8a83b1b50432e7b15c51416b"],["/categories/数据结构和算法/基本原理/树论/index.html","e1cac8bd265ff7ec7d88d5642656cc5c"],["/categories/数据结构和算法/基本原理/链表/index.html","64540ca14e3e167452cd1550ded5cec7"],["/categories/数据结构和算法/算法题/index.html","ce083a8b3f3193a94f98b252fddbcea0"],["/categories/数据结构和算法/算法题/二分查找/index.html","4dcedeeecdd0b67b53659886a59d06ea"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","0e805baece8284c49f6a711e028b7d3d"],["/categories/数据结构和算法/算法题/动态规划/index.html","33f325463aaf21d8e09b661a8d7f2915"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","de5c831389da51314c97b33874c9b247"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","03cf0dab3b9352811f0f51930a2c37c1"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","8ab28e0e289f3fa629f0977921c58b1f"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","56d8b6466d6ebda393b75f513ba5131d"],["/categories/数据结构和算法/算法题/栈和队列/index.html","15de90018affb62e432f540b966f43aa"],["/categories/数据结构和算法/算法题/树论/index.html","2595886177347a310178cc7152c7c698"],["/categories/杂七杂八/index.html","1570c80384e0de11be10cd8a5d090bc1"],["/categories/杂七杂八/博客搭建/index.html","7c93031a6ed8266879a1eb9b552a8f50"],["/categories/编程工具下载/index.html","e49fa8c8efe09adecda1ed962c4c0b66"],["/categories/编程环境/index.html","3b58b352c502d26db7b9c4af5c3c202c"],["/categories/编程环境/大数据/index.html","0c3a99f51171b0ea9bd00ea55d8f53c0"],["/categories/英语学习/index.html","b088d390161cb3b49f8a53cfa0fe820f"],["/categories/英语学习/英语语法/index.html","59e90654000fa3790589b8796dd51c34"],["/comments/index.html","42696b7fa8a50917a4f71d27be842dc5"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a0ebedbf34495b88cbfc00c8c2297236"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","77b61c568bd8bddf9851bea2c3755c62"],["/movies/index.html","b308a88ee5f900528e605b0edd78d8f6"],["/music/index.html","901782954974f7c0b7de45789f407354"],["/page/2/index.html","725123348630b6e302e2a6774143377b"],["/page/3/index.html","23f7a54fb3fbdbbc063d2889c139e949"],["/page/4/index.html","d163245388fdd168ec8bb46de4c30455"],["/page/5/index.html","f81b3e8e31d5c90ead8691bc30daab24"],["/page/6/index.html","d262d8b4bef1c7e6df65e326dbbc24e6"],["/posts/1021360842.html","1c9a00d18a0c8ab07314c5faaf6caf16"],["/posts/1120620192.html","eaabdde51db5c6203c9aa8e6e47cf3cf"],["/posts/1141628095.html","ff95d4da1a2f742e8283277c6da2ac69"],["/posts/1168613674.html","4aef33fcd3aa84d0a22f032a8ea5ce49"],["/posts/1219920510.html","d2ea7819d9f11bf09db6f75442d56eb1"],["/posts/1222166338.html","38e4159d57c2f5c8136f6b9418896edf"],["/posts/1259097482.html","0da11975de162eadb0e4a3220003c22c"],["/posts/1271036369.html","979ad093b669321ea0f4799525bbb094"],["/posts/1312847445.html","6f2f3380e229afe1983bab996c98fc3c"],["/posts/135355774.html","f650f718f534be6b57166c2e3ea6c2dd"],["/posts/1375344716.html","b47801a0d00f81a5eb5148bf5aa985b6"],["/posts/1388991698.html","d70419c6c1735eaebdd72d7f60b3b48a"],["/posts/1410315814.html","062127d5f82eac3c48b2a55899028db1"],["/posts/1452790229.html","ec21cd4f04dc2699d38a5f0d5049545b"],["/posts/1470079884.html","ea93eab61e36d31725dcd3c6606256fb"],["/posts/1470079885.html","87c4786c149ecbda69c6e45588141e18"],["/posts/1470079886.html","f3c4f8d54e5b5beec99ee2e8a5c7d42c"],["/posts/1470079887.html","87ae4593c544b6e1d04022637d7aa03b"],["/posts/1498536549.html","a9e132bbbbc29766228a556c65bdffa7"],["/posts/1547067935.html","07b0ad5843e0280ae3b2726a9b4e4045"],["/posts/1557866301.html","939e0f2db693bf41b54ae6a9106e4eb6"],["/posts/1571776361.html","de070fe1267a40174f0f59d48829f513"],["/posts/1605124548.html","8dbe804d929516f04eef32496209806e"],["/posts/1633036852.html","cf80c49b10d21048c82805508acfcd28"],["/posts/1674202625.html","d996878e83d9345d477b704b6ea00207"],["/posts/1765123828.html","03d5eeb53090c2098209888f1ac6f53e"],["/posts/1767336200.html","0854e8ac26f11e26926b60647c713888"],["/posts/1776114197.html","143a5c19d6f916f9ae655b389a346096"],["/posts/1817748743.html","a015cd245b8f20caa61d0b246b543af8"],["/posts/1925125395.html","a8b8e2527cabcaa7888dc4d9eed76471"],["/posts/1966191251.html","e7a4012a799b052751db7aa57404275d"],["/posts/1987617322.html","692e7a5d9e117707f8eda33077312f15"],["/posts/1999788039.html","64e51a88c6d9cb93dc025b8550e10dce"],["/posts/2075104059.html","115863dcdb17d132350c7dc9ba60a710"],["/posts/2087796737.html","8f43f349cae65ca1bad072791f99cc06"],["/posts/2106547339.html","0c342dcb08ceae0b4b58f25dbcc8822f"],["/posts/2207806286.html","aa086acf7af58a4a1f59f132ec052800"],["/posts/2225903441.html","05470ac79c44e45c13ba38f0aaa6b60d"],["/posts/2265610284.html","518a98c4d9fc68e7c1a01af4bebea07d"],["/posts/2281352001.html","12072a44d7cedaa0e67707270b0ce6af"],["/posts/2364755265.html","b4a3e1677691a45189a3ef4ec154845c"],["/posts/2414116852.html","2029819d72f92c34be657dc15f8e38da"],["/posts/2421785022.html","58018b1ce7a28d2e3f28c4b2cc43182f"],["/posts/2482902029.html","1f61ee73ee0088642c792f7bf38bdc4d"],["/posts/2495386210.html","11bd6e3c58883d26fcc0d9abd174346a"],["/posts/2516528882.html","4808ac6e270e324da22ab6dd135462b3"],["/posts/2526659543.html","489ddbd8b2c3eb97c4933970fd460add"],["/posts/2529807823.html","bbe4cef14d572ff8ad28f3d6fa1b33d3"],["/posts/2596601004.html","b262512c6497a5879723f936641c2c19"],["/posts/2742438348.html","c99c560f5db099b0ab163ebe6455c5d1"],["/posts/2888309600.html","aeac85a3f999a9f22b8beb64fdf1b085"],["/posts/2891591958.html","55f8d0db0d3886bebb244cb06e96053c"],["/posts/2909934084.html","4dfb59b2ae5b2f21f0db7b6b197e8f97"],["/posts/2920256992.html","037b50c36dc0d68e9a34c0402e610393"],["/posts/3005926051.html","b798c12102db94c1824f30357609883e"],["/posts/309775400.html","07a5b2077b022326d4c3e881220dee1b"],["/posts/3156194925.html","6826e64b648b37256a39fa3e93a72c1d"],["/posts/3169224211.html","082cb9580b45a13d0130fc0c43bb01f5"],["/posts/3213899550.html","574ab89c8adc520198bf3d29aa7cfea2"],["/posts/3259212833.html","c7603fc3405874af3019d223bfd3bad7"],["/posts/3266130344.html","32d955ff22a1683c9b8fce89fe3288f4"],["/posts/3292663995.html","650ed3275cd086e9eb479ea8cfd23216"],["/posts/3297135020.html","cf04ae127184118cefb7821c03d17234"],["/posts/3306641566.html","1d1c67b5b0ee1a46cd3615f15d3732b9"],["/posts/3312011324.html","af66dc6c2c778582ad77979656f3ba24"],["/posts/336911618.html","00cc6366994544b963ec4494ce91ece0"],["/posts/3402121571.html","436df028ff083aa783f764282d2c8982"],["/posts/3405577485.html","0b30abdaf7457f164240fc1d87e76e87"],["/posts/3498516849.html","89e4f712f34f21a0d38c0798dff989ee"],["/posts/3513711414.html","0de5d805f345939be39c2e83fe115818"],["/posts/3546711884.html","72b2f6342c81519f423179745f5346f0"],["/posts/3731385230.html","25d9017903195ef5197efab826eda35b"],["/posts/3772089482.html","c361d54f93db2f9d424d7c56206ca925"],["/posts/386609427.html","ed8902da66f940f8d4efbd3851c3561b"],["/posts/4044235327.html","64dab8da3cf2e1f2331aee216cb0c373"],["/posts/4115971639.html","3572d9cbe2edddac422297387025d325"],["/posts/4130790367.html","2cd6539530904e8721a5c56251ac1492"],["/posts/4131986683.html","a1d0c11a360a60f30c63996c7a613c2f"],["/posts/4177218757.html","ff4c59623d662f3b2300dc453d3dd3b9"],["/posts/4192183953.html","b78117a38e426b2771b735d20eb02021"],["/posts/4261103898.html","001085e49b2f1d6d00e03b42295663bb"],["/posts/469711973.html","aa1e59540216cd360192b24c38cee980"],["/posts/482495853.html","d7063f7ba4dd19e77f920be66a248728"],["/posts/488247922.html","3da9e5708a18583be05b39ded38b4561"],["/posts/517302816.html","24a95d1888f8808cbca35a61ae5a3939"],["/posts/570165348.html","bf9fa1beb5d0d26332dc7d35fc12b4af"],["/posts/595890772.html","e2b5ca5382a1a8f5fb80f94d18babaf5"],["/posts/67485572.html","980410b50148e81922140313cfda5e88"],["/posts/694347442.html","9669614b561b08e1404fe2e064dd7248"],["/posts/707384687.html","d641dbcb365732014d17eb0de3112bed"],["/posts/71180092.html","ebd79e98f4a99b70eef79bce77496b7b"],["/posts/716459272.html","61577c727404709db7da4be2ebecde2f"],["/posts/778231993.html","9e9ee52c5ace2530df37fa63ffd5505c"],["/posts/795397410.html","5c0ea1c86692e77ca504f4b092a9b2ec"],["/posts/820223701.html","f7cef829699c0bc9e9e1a2d1f4f31641"],["/posts/830372185.html","c60dfd80420e50d0cbcb9b0c45f91619"],["/posts/88294277.html","3976f8865755753088e9207e8ac8253c"],["/posts/939963535.html","302d9796cb0f6fa3ad4e4d1c1943def7"],["/posts/983786067.html","c1138e59b4211aa12fac46dd04ccbdf3"],["/sw-register.js","86b5dd3e9dc66185f5f8b88fe48d1361"],["/tags/C/index.html","3c65f020cb8abfbd0692eb727446e4c0"],["/tags/C/page/2/index.html","95a27a31b929111fdf385c9f78dda256"],["/tags/C/page/3/index.html","fceb502af80bdfebc1105f044bfb5b70"],["/tags/ETL/index.html","67e1f62e12295a48925f6aa54f8d027b"],["/tags/ElasticSearch/index.html","298daae573461709ca860db8dc23cff2"],["/tags/GUI/index.html","373fc971890912d6e76963badf28e267"],["/tags/HBase/index.html","b99fe6cce4c309b89964bcfb878a8c51"],["/tags/Hadoop/index.html","fb7e1bebc682017ef7b1056ab739d8f4"],["/tags/Hadoop/page/2/index.html","48581b35fb02d4b6fe22246ebda46878"],["/tags/Java/index.html","83317ef5fdc90e57d7e6db683b0de82c"],["/tags/Java后端/index.html","416938b4a184c7f50388b5b66ae9df43"],["/tags/Java后端/page/2/index.html","eec322b1f75d4a5a40027aa47dafda59"],["/tags/Java基础/index.html","bef053978b4b10a5a5d86087bb121634"],["/tags/Java基础/page/2/index.html","c1dbc109cd0e5d07099311cd650e9dce"],["/tags/Kettle/index.html","afd745fe8ce89e29805a8b2602ba2f30"],["/tags/Kibana/index.html","9b41e1cf8b0491bc70d704e7547eb725"],["/tags/Linux/index.html","0fb9a8d4d83f07a33a4e5a1b22a5576a"],["/tags/Linux/page/2/index.html","d10c5a63245056df64a21575c23cc21f"],["/tags/Linux/page/3/index.html","2553626bf0a0e6468cbebfeb51eb9ee2"],["/tags/Mac/index.html","0940218f4770b2193382dfb64ddf009d"],["/tags/Mac/page/2/index.html","cd7b327d515e1b75156a2ba5664c511d"],["/tags/Maven/index.html","5be47f474186104575cacb34b09849b9"],["/tags/MySQL/index.html","fdb4e3df17f1926e5f951aa64d2f6299"],["/tags/Python/index.html","1563d8fce269061d6d3ae031a4983d86"],["/tags/Redis/index.html","869404c6d01a19a45241d8d121cdd4cd"],["/tags/R语言/index.html","67cb4f23617ea8efcfd6ee0335004477"],["/tags/Spark/index.html","aa52a2e45bbafbba3927f25d924e96ed"],["/tags/Ubuntu/index.html","758177d16188128337c644f512405de8"],["/tags/Vue/index.html","818404972107830f908439cc7ccf6a46"],["/tags/Windows/index.html","1ede788bb3b342b070f17f421b009908"],["/tags/ZooKeeper/index.html","e9e01c238473ed8c99ff12249c691252"],["/tags/bfs/index.html","47e797210b3be5dd3c263e55cf463f14"],["/tags/dfs/index.html","3f68a77ccd33900bfb6fe6f7b9feee41"],["/tags/folium/index.html","66bfa8b3331ff375bde7a7527f611ff6"],["/tags/git/index.html","033d92da0391418eb1972817d79c3faf"],["/tags/index.html","a933131797f3e718d13c21c50f2a66cc"],["/tags/latex/index.html","92a05d505180a21cea7725f0f0133108"],["/tags/中间件/index.html","20fdc9d0ddb49c8975cb59d25b5e8055"],["/tags/二分查找/index.html","5c560a12aa4fe93de3ce2fb5d93dc093"],["/tags/优化类/index.html","65a317fd9c8a7879e0757ce2ef4520d6"],["/tags/前端/index.html","bc3a5430e1dc66ef571a73fd233c038a"],["/tags/前缀和与差分/index.html","c904a42ad3a9e0e7b8abbbf9b1f019dc"],["/tags/动态规划/index.html","1af099b5951b49e933976e4e5f1e892e"],["/tags/动态规划/page/2/index.html","d76931c2f5552e982f503d8ab344963a"],["/tags/博客搭建/index.html","f46065324e05715c7ae29aff6f1ec639"],["/tags/图论/index.html","c03ef8a7ec5dcbb7c7d5ec82415b2e3f"],["/tags/大数据/index.html","89592c43fc98132655a82b50fcf37176"],["/tags/大数据/page/2/index.html","dbe7693d5ad1e9ea2097d0756bd5b95f"],["/tags/操作系统/index.html","ef7e7a6e6f6606682e590351d41e6a34"],["/tags/数学建模/index.html","177681c16b9399bd5b161bb0a699d741"],["/tags/数据库/index.html","044196709e036194df28fe89a4968112"],["/tags/数据结构和算法/index.html","82e6ff3b0fc9d3ba8fbe82ffcb884ad5"],["/tags/数据结构和算法/page/2/index.html","8e11522ad34c1096df49d2850e4b9ec9"],["/tags/数据结构和算法/page/3/index.html","62459b3544fa1b57e8fd1da38fd6076e"],["/tags/数组和字符串/index.html","4ad142dac11d6395119450ecfd436fd4"],["/tags/枚举类/index.html","6a45bde8a8a89ddbcd2348e5cf7a8b99"],["/tags/栈和队列/index.html","b52a9de7d8a24c1ebfbb2e15e720151c"],["/tags/树论/index.html","b710d0892acd1c91170f7dd563124e94"],["/tags/测试/index.html","51b8198835342ab6aa32266a9ac0924c"],["/tags/环境/index.html","eb74a6f0166e3ceb281334acc2b3a7e5"],["/tags/环境变量/index.html","663cc9a0b8a131075f8c2ce33a17c156"],["/tags/绘图/index.html","5144c22ac8a201f04717e5d9c9d93fbb"],["/tags/编程工具/index.html","da1db939f0f30c8389d0d085efc98bc6"],["/tags/编程环境/index.html","74eaafccd5e0adbe4e62c2164d2a5e2a"],["/tags/网络编程/index.html","88a6508446e723c348e8640d8b347197"],["/tags/英语语法/index.html","de10665fc3c09092a16eee4eced4f9cb"],["/tags/论文/index.html","53194ae5b7c3c203dd377702bd66f62e"],["/tags/资源下载/index.html","744bfb4d95847c30c57e2f6d7902105a"],["/tags/链表/index.html","8011a28d657b2c6afd1da91774a3d832"],["/tags/集合/index.html","32985e9abcfa65f818a530faad63e2b1"],["/tags/集群/index.html","287035bc536b48fa91738500fda67112"]];
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
