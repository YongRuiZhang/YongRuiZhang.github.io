/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","03b590ef56d56ea3dfda0505853f8578"],["/about/index.html","0bc5fe9554ddb291c12b9ab38456bbbb"],["/archives/2023/01/index.html","44a464710aa4b5cb295a4c28a4252aa3"],["/archives/2023/02/index.html","4e13f74234cd8657a7a2ac6a1618384e"],["/archives/2023/02/page/2/index.html","a81d1622a410554e15316a2473783b50"],["/archives/2023/02/page/3/index.html","5119266fb876d077b8c50c2739924d9b"],["/archives/2023/03/index.html","efe85405b4780c2819009a2b385ff268"],["/archives/2023/05/index.html","4dd0720f09aa3f23e4ac4dbf02c0b232"],["/archives/2023/06/index.html","26961e0000e33f69ba5b2e42bd9b2d6e"],["/archives/2023/09/index.html","a74b4e7e0068c26b04a63bffd888b6b0"],["/archives/2023/11/index.html","08a948d67c0d2a4a08aa06aeb1459770"],["/archives/2023/12/index.html","b8062915387f34ce35c043cdc4642446"],["/archives/2023/index.html","55d80f2ad18b716757d66671a81b8e3a"],["/archives/2023/page/2/index.html","3efc79cb31c9f0fb35cdd66002fbca2b"],["/archives/2023/page/3/index.html","d30db730571a1d56885f85d013d92706"],["/archives/2023/page/4/index.html","8bdda0112f494d9d3d49c560c710c1e9"],["/archives/2023/page/5/index.html","bf2965f5d6de777990355a18a5aa4a84"],["/archives/2024/02/index.html","f273d796cf1f22764d2ff6b4ce7173c3"],["/archives/2024/index.html","c43d49d95fd38c9baf83a02f575d2fc8"],["/archives/index.html","98004d36dda51144dc33900c95fea028"],["/archives/page/2/index.html","198e406cf984ad4dc9a083034bde6138"],["/archives/page/3/index.html","3843c539b0ed4c07c693d6454695307d"],["/archives/page/4/index.html","a1903dd26f91cbb297d79f2ff3ff761e"],["/archives/page/5/index.html","4035ce63d7623ce1fb428af38af36049"],["/baidu_verify_codeva-qQP2iZOMLX.html","c862e8134c664b862ebdb30970bf232c"],["/categories/Java/index.html","d28c8757ee0669bb0cdfa2ec72c059c7"],["/categories/Java/后端/index.html","abd513252980407e789ac7fcef4cc439"],["/categories/Java/基础/index.html","94087a3982397b453823284ecba8c8b3"],["/categories/Java/基础/集合/index.html","76ad2afc23972fdc5f1ca6cd72dd4ed2"],["/categories/Python/index.html","f333d835974b959ebf84898234a07ccd"],["/categories/Python/编程环境/index.html","a7fb70efaf9289c4a6ca6871edf59128"],["/categories/R语言/index.html","59fd7370ee191950aa914a49a632ca31"],["/categories/R语言/编程环境/index.html","d2862749bc1abacca1ef2dbd9c828ac1"],["/categories/iPad/index.html","af9b8064fe7f6315c1f2bf569d17fa6f"],["/categories/index.html","8dd7bc759a2b15851d368aa290b2bfff"],["/categories/中间件/index.html","512840b2856f3cbac2ce90a0c039dfd4"],["/categories/前端/Vue/index.html","8e12b912221e65531cde22ac67361aa5"],["/categories/前端/index.html","a1f76eea09ca4355efabfc7cd638c37a"],["/categories/大数据开发/ElasticSearch/index.html","1dbe4487fea08a69f9b7546759105992"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","96eef6a841c91cf7a1f243e8719d714c"],["/categories/大数据开发/HBase/index.html","0da52e61e690007892fcbcccadc36ee3"],["/categories/大数据开发/HBase/学习笔记/index.html","c42064a4f30f5791de638c11bc48cbeb"],["/categories/大数据开发/HBase/环境搭建/index.html","14078e8dfa27c22bb7f830ea0f003625"],["/categories/大数据开发/Hadoop/index.html","fcec155ca7a54eaadc9628de4bb39bef"],["/categories/大数据开发/Hadoop/技术/index.html","18c2565a8a658232804140ae09f78b89"],["/categories/大数据开发/Hadoop/环境搭建/index.html","7787f837899477742a104d466d22d7e7"],["/categories/大数据开发/Redis/index.html","3f7122f01af2843af29bb457f34e26c0"],["/categories/大数据开发/Redis/技术/index.html","fb4ecb8a44b0e133fa404534070855a9"],["/categories/大数据开发/Redis/环境搭建/index.html","6a14e4c11c12039bf3de7725546e044a"],["/categories/大数据开发/Spark/index.html","949bd85ce9f1e9936ffb5ca8bbbe1d14"],["/categories/大数据开发/Spark/环境搭建/index.html","dcff6c4a6fe2570bccdd38959a3a41af"],["/categories/大数据开发/Zookeeper/index.html","d49e8bce6778a28f2d8cf68136c2ebc0"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","df6f73f96c0f36b6e4e3fa3060c5b9db"],["/categories/大数据开发/index.html","54f62d3ce219e0f234b89b5498e6e765"],["/categories/学校课程/index.html","61d6d2e4a6b05fbb324aea1814ee0acc"],["/categories/学校课程/计算机操作系统/index.html","7796c4d454918eb710c83b2fa5dd2753"],["/categories/操作系统/Linux/index.html","2370a5b2e6fd7511431af7af14847f34"],["/categories/操作系统/Mac/index.html","6f68146e1a07a8a4b372997e55814eda"],["/categories/操作系统/Windows/index.html","b90602b550f9df0b00a63aa4ae5898b1"],["/categories/操作系统/index.html","4fe97ce4efe9ec375144ad404677a515"],["/categories/数学建模/index.html","8b32961bd05fdbf79460945f74b9bf43"],["/categories/数学建模/latex/index.html","499f5b68157fcc1348e0dfbee4526fdd"],["/categories/数学建模/优化类/index.html","064cf9cc9e8c1cf7ec93f437733df041"],["/categories/数学建模/优化类/现代优化算法/index.html","c30ddc773c5295f24a7ab472e58db13e"],["/categories/数学建模/优化类/规划类/index.html","c01fade1f0c3bc4bdf9846089c1299f9"],["/categories/数学建模/绘图/index.html","56e7d74f580b741c25a70cea6556a743"],["/categories/数据库/MySQL/index.html","b723e524b83719f0841a2a16c37c7706"],["/categories/数据库/index.html","2da0f05e5aced469f24375111038dbd9"],["/categories/数据结构和算法/index.html","611a63abfd31b240f68e739291525b7f"],["/categories/数据结构和算法/page/2/index.html","7aca370ab9933e070f67ae0b745fb7ce"],["/categories/数据结构和算法/基本原理/bfs/index.html","686b1e6ca36de3390715f95008a69a3e"],["/categories/数据结构和算法/基本原理/dfs/index.html","04c33758681bfcea5014f7c5bc7ae5aa"],["/categories/数据结构和算法/基本原理/index.html","66fef444e58f3f0f0d5c769b7e1ea040"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","d051d27fc84a41ff994e1f34f8c8395b"],["/categories/数据结构和算法/基本原理/动态规划/index.html","236f7d8ad5c10e8f19c43f7baec6f287"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","3f49c6dddac462e302e971102cd10fc8"],["/categories/数据结构和算法/基本原理/图论/index.html","2622cddf5d7f0df65819559b22638c86"],["/categories/数据结构和算法/基本原理/图论/数论/index.html","d12697e836de3ebe5968283a497efede"],["/categories/数据结构和算法/基本原理/图论/树论/index.html","bb92b020f9859fe0548930dc8ecaeceb"],["/categories/数据结构和算法/基本原理/字符串/index.html","16dc38a5d0021344796f2f5b0495ca46"],["/categories/数据结构和算法/基本原理/宽度优先搜索算法/index.html","bf912ea9dee9a45c459c5062282bfc36"],["/categories/数据结构和算法/基本原理/排序/index.html","06068e695ac25a6527047c0dc5bd890a"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","1e7a62a581676b127abdae59f196cd9e"],["/categories/数据结构和算法/基本原理/深度优先搜索算法/index.html","1b03e6047a18c967f2bb8494604fc2b8"],["/categories/数据结构和算法/基本原理/链表/index.html","85c5007b6309e764836c4128105989d8"],["/categories/数据结构和算法/算法题/index.html","57e2989e335ee7884e2c11779ddeecac"],["/categories/数据结构和算法/算法题/二分查找/index.html","5dacbcfdb327bd1eafea3bb0c83e3bb9"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","11e80c24fe72fa75fe301a661b6448f9"],["/categories/数据结构和算法/算法题/动态规划/index.html","d61e67090ad1a26ce06e8b4e01783448"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","e117c6f3ff0646fd59e2069288a09bdd"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","7dbfd2fd13d0bdf66bd86b3c7d58a25f"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","a55f9653da561b50c4b08fb300addfe6"],["/categories/数据结构和算法/算法题/图论/index.html","b69ad36d82ef431872324eb14d09461a"],["/categories/数据结构和算法/算法题/图论/树论/index.html","156e9ed6e5fab0b6ad7b20c14508b746"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","4bce8e1c28605f2167cbe5febd3498af"],["/categories/数据结构和算法/算法题/数论/index.html","e1fb6ea85f5eb3e3a26954d9711fff56"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8cd845ee5ef1ddfdce5f2508adee0d2a"],["/categories/杂七杂八/index.html","57d0b7a755a6c28fba70aa3fbbff4157"],["/categories/杂七杂八/博客搭建/index.html","34c97f180597911af95e059aec7395e5"],["/categories/编程工具下载/index.html","43953735a6276f62cd819cc6d866a300"],["/categories/编程环境/index.html","495893d88210e2d92fbcb712056e8144"],["/categories/编程环境/大数据/index.html","6f3870c17b7a8e5ec364fb352fbda654"],["/categories/英语学习/index.html","d8b38dd5a3c6d762e1b31796945854c7"],["/categories/英语学习/英语语法/index.html","9944909cabafda867c545408c7f88335"],["/comments/index.html","a1de0db43dcb2bcfed22d1636434d394"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","a236b3bb9c40edc4a3c876c87eb0a330"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","44918412c4b4c2e0aef4737a2213de2b"],["/movies/index.html","39d65147c49f425782f9fb890efd5b93"],["/music/index.html","2d53cf827de004d8737667b55becc052"],["/page/2/index.html","e7ca5b8adcc88b2bc57a678ca86b869b"],["/page/3/index.html","f1e907e27d230c5a4c045cbb5979f29a"],["/page/4/index.html","bf0e079f2940bf0218ec547482adef43"],["/page/5/index.html","8b964478033a031c155343e027ffed2a"],["/page/6/index.html","9f2872bc3a14d648014933df563b4bdc"],["/page/7/index.html","240d6d158a1d45d03e5ec4ddde098151"],["/posts/1021360842.html","c9ded2a643a85d4cb3e166b13523b40c"],["/posts/1120620192.html","a6ab44080743104155eddd78c3ae77f7"],["/posts/1137707673.html","49854e5b84235924a7dd64efbba37952"],["/posts/1141628095.html","be9270a216be7181722728c4852ac0e3"],["/posts/1168613674.html","23e9b18ce620bf4090243f4381ed5994"],["/posts/1219920510.html","13b872fe4f82effeb4a2efcf890571d8"],["/posts/1222166338.html","b1dd088d23cf3bb5869711dcaa2ac41c"],["/posts/1259097482.html","c7fd6ded1007ca8ed2b02b6b91f7411d"],["/posts/1271036369.html","1a9127f266b25e9021aefe09f71fdc29"],["/posts/1312847445.html","ccf4376edf847495c81c0ba702d6658e"],["/posts/135355774.html","10354f6262c080f8070ab064a16d322e"],["/posts/1375344716.html","ca66f1d13f7699d0d67322668f6d8997"],["/posts/1388991698.html","4f71977788280ed750085257c57d92c1"],["/posts/1410315814.html","7a0b7f27562573e96884088e4d4ea273"],["/posts/1452790229.html","e2d1dfe5a19ada91562d2ea1e1e0825a"],["/posts/1470079884.html","3295532aa56c9f73852fbb37c36888f1"],["/posts/1470079885.html","e1b0e746327a11326f87a763cda07d32"],["/posts/1470079886.html","8d9ef1787be7d995982728155f6dbeed"],["/posts/1470079887.html","e13808ac5bd930c10cdc37f108e3db40"],["/posts/1498536549.html","96eef2c1db04a7112f417f8c6ade535e"],["/posts/1539568593.html","ff8499e06728e8819a73752f1bea4f2d"],["/posts/1547067935.html","c872e9028cb3b2f7d7ea0667167ea6c3"],["/posts/1557866301.html","88a631507f70e99f8000c88da24a1f8e"],["/posts/1571776361.html","c53b1f7364d7b55f382f32183e21c1a2"],["/posts/1605124548.html","6a297b33a6ad3c82b28a72d34a73188c"],["/posts/1633036852.html","60ac567b8380a6384f7b3da0f468f617"],["/posts/1667740714.html","0a7ae0d1fa3528d894366d2a78031096"],["/posts/1674202625.html","344f93137d090bb88a4b5edaeca9ce32"],["/posts/1765123828.html","ea6b5d1720605f87683268c4b5e76121"],["/posts/1767336200.html","6b8d0ed9f39dbc1537332b3e0f87e7c8"],["/posts/1776114197.html","43f1f258562e5b64a11954906329397a"],["/posts/1817748743.html","3df56a54fbaf44ee22188127a95b9abf"],["/posts/1925125395.html","50538ba1c93d4c99bf1487e3e294116c"],["/posts/1966191251.html","90d30972fe895166c0f8fbd78e4d31ce"],["/posts/1987617322.html","10bae93180e2a092c2be2f4e984f1aa2"],["/posts/1999788039.html","51384050aee026287e9a7f8f74a11c66"],["/posts/2007534187.html","36ee4a2601409e548b1c551086dc4398"],["/posts/2075104059.html","27ac318418ffcdb9623d97a75a1f1dc5"],["/posts/2087796737.html","b435858b5701bbed90a70c0359945a85"],["/posts/2106547339.html","c0e12cfb4fa42b88cc8e2e1f0a8b9a8c"],["/posts/2207806286.html","0caf93c092e844c1084f17ddde4ea9c7"],["/posts/2225903441.html","53f43af9145586a10c682fe6af55044c"],["/posts/2265610284.html","542c3a1cf82265949c993b4b88c00fbe"],["/posts/2281352001.html","da96925451e3c1398e27d2f38d307a48"],["/posts/2364755265.html","38584b5c0ddb192c969223dc6eefe7ee"],["/posts/2414116852.html","e3f1bd51a2e62b07be23a49b40eb726e"],["/posts/2421785022.html","880c87fd189125ff2dc0b46efac05ec4"],["/posts/2482902029.html","5b6419abd31aa9304eab8bf71f5d7c78"],["/posts/2495386210.html","835cf4382120f69a41a6cace713a7e5b"],["/posts/2516528882.html","bacdcc4320f10efba099c5186371153e"],["/posts/2522177458.html","824374cf1e09cbe10c87aab4349d592d"],["/posts/2526659543.html","0eb113aa314229487a67defdb6d8ac96"],["/posts/2529807823.html","ff7cc6f1ab0e14849765abf3c0b74194"],["/posts/2592249117.html","c0150d2c76a85c8931abfabad81540b3"],["/posts/2596601004.html","91700cbd318a3f6691407c403ad49eda"],["/posts/2697614349.html","78af32f46a1c0c276d60da23bff393fd"],["/posts/2742438348.html","c4fec5c0083032b79eddb20981066244"],["/posts/2768249503.html","4a2f1775a3e56b4b2e639a112393c592"],["/posts/2864584994.html","83249aacd256eec485b8fda57e53cb95"],["/posts/2888309600.html","f81ab4c860760f3d3e19920fb1fa0cb4"],["/posts/2891591958.html","82c93f1a9aebb6328337a0e1a2f1e79c"],["/posts/2909934084.html","9f46c78e1c85ea4ce9836d50be78e753"],["/posts/2920256992.html","e8153a4d6fbbd1e86e080d78bf4037d9"],["/posts/2959474469.html","7de952c15e52df7ff71da66912652e9f"],["/posts/3005926051.html","8ad0987f2201553d8d96d9697e3f7699"],["/posts/309775400.html","2205c5ca41e541d739249fe3ea0a15ac"],["/posts/3156194925.html","c356201c8ccd7f1230b41c4b8b714747"],["/posts/3169224211.html","f0ab8cb1d0ec8dee3f9223436cd98bd4"],["/posts/3183912587.html","ffd365fd7c4d66f9891725c11007673e"],["/posts/3213899550.html","f74e41ec1575aa1c0e0c0f4ec157e1ce"],["/posts/3259212833.html","97e42502c150dfc42705a0200212a7b4"],["/posts/3265658309.html","1e878e584f06f96db636488a25955076"],["/posts/3266130344.html","ae43e1d016d154cf8fa8ab8b3dc80ebb"],["/posts/3292663995.html","3f0669f60d1576c59d09ba4b67ac33e8"],["/posts/3297135020.html","f15e10c58c218e1825677bd4f522d6e2"],["/posts/3306641566.html","20b98b074463f9b96caa2df7fddd5333"],["/posts/3312011324.html","9e3f5229254ded1753a5a689bdd516df"],["/posts/336911618.html","335aab8e42a6045f5f4c57941174b73c"],["/posts/3402121571.html","ca4a6fb31eb2835fefbe814e3cd7e01d"],["/posts/3405577485.html","b0c34da2793b41458e73b91270b8cbeb"],["/posts/3498516849.html","a78843f336505026424049e206900f19"],["/posts/350679531.html","822b4f0db0428bf68d76ccde4c32ebb0"],["/posts/3513711414.html","3718af87ddb27b523d031d2e284bef46"],["/posts/3523095624.html","85a8076b100fa2ed932e888139f8c2a3"],["/posts/3546711884.html","6cf8df6afcfad6e349478e3aa7845e5b"],["/posts/362397694.html","7ad1a3aa163a9b9cb71b27cc34f75046"],["/posts/3731385230.html","cf99db2509bc70158f95207ad6ba9e27"],["/posts/3772089482.html","787921ae67fe8359c275cf17070ff21b"],["/posts/386609427.html","102273384a451b95a8a6a4be8d3cf843"],["/posts/4044235327.html","7a537a769dba15585b40de7d50474235"],["/posts/4098221856.html","90225867cdc5dd4a04bf58fa92bac792"],["/posts/4115971639.html","e96a75197a1fe8981da990950590d45f"],["/posts/4130790367.html","864a33401fd1a7487943a368c98ff773"],["/posts/4131986683.html","8d95998dcd73fd367a06135addf4ba5c"],["/posts/4177218757.html","feda36fbd36eac855f67b2e1ebcec14e"],["/posts/4192183953.html","08648844cd9da91c62769f5fc4dda81e"],["/posts/4223662913.html","d9fb4f9740fb0db73bdc958e25a10d86"],["/posts/4261103898.html","b143cab7ca5256ab95c52c2dbeb65a35"],["/posts/4286605504.html","2616bcfcaa60b1597b9649e3f94c1ef4"],["/posts/449089913.html","d516349cf55abd357a3317dce212dbc7"],["/posts/469711973.html","025f3b534052d0970d4c03415da58bb1"],["/posts/482495853.html","387f985a69ebae62ea6ea2e652fb36b8"],["/posts/488247922.html","65069876ad3d63c2b0c727749f7353ea"],["/posts/517302816.html","af0eb2c8f2baf09a431709182605b254"],["/posts/570165348.html","bc5018533ab591e5e016e95c087f6874"],["/posts/595890772.html","d4e1819935199c1d0b3b9fd136cfc23a"],["/posts/67485572.html","24fa1bc31c6c9a8d5363736649af08be"],["/posts/694347442.html","eea2153f5950df0eb558b469c67066fe"],["/posts/707384687.html","86642db7e0d8d2643b9f1e9ca97aad99"],["/posts/71180092.html","09226afe27258af196de00e976568d14"],["/posts/716459272.html","1b3f69742d1c8f6ca5ec10de3c0d3c28"],["/posts/765481613.html","8a04c4831c4ffb4e9ae54359112c4020"],["/posts/778231993.html","a2465eb18ec609a7b57b0750c4766197"],["/posts/795397410.html","4d3bb713eaf210305fb70f8e501f76f7"],["/posts/820223701.html","2970270bdd667755963597feb7fee4f5"],["/posts/830372185.html","da8a193fc08ab699895f91c2cc64bb2a"],["/posts/88294277.html","da50caa4a46e0f80ed1cbee5a82fb3f0"],["/posts/939963535.html","d9492ac0c038e41b649f3927164e6845"],["/posts/983786067.html","43f596a7eaf248560d44841e1ec438f6"],["/sw-register.js","c146de3f698dad488538f0dd9716514e"],["/tags/C/index.html","f5d522532fba04a509cfb8de5257c69f"],["/tags/C/page/2/index.html","cde8628ccb131c674923a82825b77eae"],["/tags/C/page/3/index.html","6d71c18b083e9b68d3ecc1a4786e307b"],["/tags/C/page/4/index.html","e3bd0f683530445826e0b330446ae155"],["/tags/ETL/index.html","f6ceaee326ffd20e0b9cbdf3de006e7a"],["/tags/ElasticSearch/index.html","5b7ba59e3643f42a0c4f250b6f3770ae"],["/tags/GUI/index.html","2f5456565be270d6fee775c4f7e3c383"],["/tags/HBase/index.html","4faa64aefed0044566cb29fd689d3c30"],["/tags/Hadoop/index.html","b286e59d14a85b7667b72eebf1f3da51"],["/tags/Hadoop/page/2/index.html","78cb4659fc8c69351d9048c616288378"],["/tags/Java/index.html","cd0e4610a36e43859cdf7a1493a17559"],["/tags/Java/page/2/index.html","4cfe16cf25a4c646bef2979c5d487788"],["/tags/Java后端/index.html","0c917c7d16e60b6256229eb22614449d"],["/tags/Java后端/page/2/index.html","40d0a25773dadf7dc9b697c1a888bc2c"],["/tags/Kettle/index.html","c76e1c54b6e54e8e3e454b2de5ca6b32"],["/tags/Kibana/index.html","84a36d457f95d914a9a34b13aaa9b099"],["/tags/Linux/index.html","abff12a1c9fa05fbdc2e32d0285f380a"],["/tags/Linux/page/2/index.html","7a90ecc00fff77120360abb024b472b3"],["/tags/Linux/page/3/index.html","d2712b86d6a9382de83e1cc7346efb8a"],["/tags/Mac/index.html","be21470ab12af951a99d8f42a09963c1"],["/tags/Mac/page/2/index.html","c216c4cd97ff76f04a5eb2354c0c3a15"],["/tags/Maven/index.html","43aa4aaa31970da55887f85032e1ebf4"],["/tags/MySQL/index.html","bf039f733017b4c044ff7c5dd64444a6"],["/tags/Python/index.html","d06ee3ed1f9aa723c98210c0cf857f41"],["/tags/Redis/index.html","000c515037287ed032b4967af0f7a9dd"],["/tags/R语言/index.html","6f948a20b84cfe9a21569c1f1312afbb"],["/tags/Spark/index.html","8c03cb3214044897ed1b854671f0cbe7"],["/tags/Ubuntu/index.html","fac316a2e1c933d35873598b4b6cc139"],["/tags/Vue/index.html","46ba72f7d292a753257ec3bd01bf95c5"],["/tags/Windows/index.html","aff2b7fcbde8cdb33a3800894d160641"],["/tags/ZooKeeper/index.html","16cf279caa133308a65e094ed4d6779b"],["/tags/bfs/index.html","333e7a131ee700f91fbb0f65c594f2d0"],["/tags/dfs/index.html","933f85cf798a6eaba9c2d616d0b99071"],["/tags/folium/index.html","d3a6948141498be9ce6082bedc583a63"],["/tags/git/index.html","a5bedc834e3e656db6632da2b0cccb99"],["/tags/iPad找电子书/index.html","a31a01ff725b5f46cd40a37f216a3c3b"],["/tags/index.html","ab2060ef8384a8f5eacd3c422aae3014"],["/tags/latex/index.html","b9e29e3a589da8b8e2c909c093ba8eb5"],["/tags/中间件/index.html","1c51de8200523bff7e9e222aa651e07c"],["/tags/二分查找/index.html","7da8a18c8dae8bad6b4f4f3db0d027bd"],["/tags/优化类/index.html","4f46f3bc1fb69c832bed9654374e4fb3"],["/tags/前端/index.html","68ce732619379df7063deb8c87c1fd3e"],["/tags/前缀和与差分/index.html","f01aa88d390bc56ef3d083b2e3c4d4d1"],["/tags/动态规划/index.html","ebb9096d1f341eea77e60884d37d133f"],["/tags/动态规划/page/2/index.html","8936c428b3e00fbfa9f91f417bf3ac96"],["/tags/博客搭建/index.html","d9a2f684f7ec66e08904e1f4e3a17ac6"],["/tags/图论/index.html","9d96743987a37c03429c56622511c147"],["/tags/图论/page/2/index.html","366f74a89bf7ff93a72a350b512bd920"],["/tags/大数据/index.html","8fee524358acbb48c9111d313e0961a1"],["/tags/大数据/page/2/index.html","f3025489370f6e57c3832c20cf73ae09"],["/tags/宽度优先搜索算法/index.html","6273b13fd7e73980da14276b73a23e58"],["/tags/排序/index.html","1d3a3c6acfc1dc5b37a53c076d5c780f"],["/tags/操作系统/index.html","3fabe458195ac65aa44f3ebe3cf8bb35"],["/tags/数学建模/index.html","97ddef9a07b18bc1d43e8234f4d924ba"],["/tags/数据库/index.html","cae3e84bcdc5248830346af7d40c91f6"],["/tags/数据结构和算法/index.html","5b2bcca069460afb8d0b4b8c0a08bb20"],["/tags/数据结构和算法/page/2/index.html","e0a2562b559b439a958eeea13b2ed5ff"],["/tags/数据结构和算法/page/3/index.html","10f400c24d4b205c6c328babef316cee"],["/tags/数据结构和算法/page/4/index.html","51aa092315ee64841383a3aca6386077"],["/tags/数据结构和算法/page/5/index.html","66c60dc7718b1af84d31a2cd46fb69a6"],["/tags/数组和字符串/index.html","4c561f4f1a8c9713b407d8b8095b1f75"],["/tags/数论/index.html","055f71836a73f5541d0d6e39bde90c4d"],["/tags/枚举类/index.html","d448bdcfd9011756ca6353ae238c6501"],["/tags/栈和队列/index.html","ffe030b9865312b3787eb13d7dde2c28"],["/tags/树论/index.html","7e199d2bb5ec1af1d9d21f151df40046"],["/tags/测试/index.html","aad94e9270eff6a4252f02dc939b4dc0"],["/tags/深度优先搜索算法/index.html","17dcd06333339ab1ca033f5acfbb8c2c"],["/tags/环境/index.html","79831860e09256c896939cc1f4baed0d"],["/tags/环境变量/index.html","4a9aacae5ed3b8729117d86376e4a93c"],["/tags/绘图/index.html","5699dd21661b866803f8b0540d2fadf2"],["/tags/编程工具/index.html","4152c3eac939565ec732f85889e6bb2c"],["/tags/编程环境/index.html","0e1ef611462cdd6b4857099d072a81bf"],["/tags/网络编程/index.html","16b290341c1e04b8548f26642e999daf"],["/tags/英语语法/index.html","0bc8a6d626b3f9ca12e00ae76d8fec7e"],["/tags/计算机操作系统/index.html","929230b8ed38092d91f2124e532d3cb2"],["/tags/论文/index.html","6ed5f52de7f88ca4bf714e753361a2ba"],["/tags/资源下载/index.html","67fbd6f19361695e1c4404e6cc7efe3e"],["/tags/链表/index.html","b21cf48ffb9b5381078f4e8c83aafc32"],["/tags/集合/index.html","48424070de56dd81dc74075106681923"],["/tags/集群/index.html","a49fb3ef1942f6d710c9847edbe9cfb5"]];
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
