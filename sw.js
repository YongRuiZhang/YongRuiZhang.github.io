/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","263d822038511166806290103077a423"],["/about/index.html","af39817ed62c031358bd3437b1a269ac"],["/archives/2023/01/index.html","a319d709bbae528a6f2ae1d49036569b"],["/archives/2023/02/index.html","c67d4f37a609dceb17b626357c7494b1"],["/archives/2023/02/page/2/index.html","f54d5973be03fb9182113a109109ba55"],["/archives/2023/02/page/3/index.html","a42ad191177bc65bcb390974e33d6db1"],["/archives/2023/03/index.html","a60da2392d52cc2fae3a85f5777551f9"],["/archives/2023/05/index.html","ad8f3aed299d8d8809051d550d38d492"],["/archives/2023/06/index.html","8e87d9d49089459cefc3f609e5c0d2d8"],["/archives/2023/09/index.html","609b13a99819dae8b6e205e1820b6f97"],["/archives/2023/11/index.html","a0341bc7d7cabd8de020233b9c75bc4f"],["/archives/2023/12/index.html","73c5987a72210eaf8edd0d335dbd4e80"],["/archives/2023/index.html","741d0a5bcd68f7fdcbe034bcceec2666"],["/archives/2023/page/2/index.html","a7b595aab9aa80da9895e32362a4d92d"],["/archives/2023/page/3/index.html","a6fcae40cc1c8094b84f83b90188719b"],["/archives/2023/page/4/index.html","a1fa8e7be11c089bebafc9830abcf638"],["/archives/2023/page/5/index.html","4ba3f01540926b68420f727a637713b7"],["/archives/2024/02/index.html","907b6f3e1aee3c9da6be42b3fa271b37"],["/archives/2024/index.html","0fc3808a0fcfbe23edebc7702b0626c0"],["/archives/index.html","4315a18fb087f5ff92d685a15d857625"],["/archives/page/2/index.html","1cbe05491e5896ca9d98e389e49d0681"],["/archives/page/3/index.html","f2a89f09fadd143e9c3138ad091cae9b"],["/archives/page/4/index.html","fc34739eb91c6f36475d3f957c9da2d0"],["/archives/page/5/index.html","1f33fa05988e0004a00304d0e4d6d002"],["/baidu_verify_codeva-qQP2iZOMLX.html","0cd81e758a879770b03762f204dbd39f"],["/categories/Java/index.html","5ee86edc66fbf1ec95f5ac9d9449603a"],["/categories/Java/后端/index.html","406d38775c53b4b92e959cb2712e28a6"],["/categories/Java/基础/index.html","d9aa973879cc392d78a2565b928605e6"],["/categories/Java/基础/集合/index.html","caff4df10a478a49af1a1c61b7199f7d"],["/categories/Python/index.html","1f200fc1cac528e8d76bae6a15d7214a"],["/categories/Python/编程环境/index.html","3c037e92e4f441397e27e1542d4ec7cc"],["/categories/R语言/index.html","c588a39e3cd8b55cd6c37cb76083846a"],["/categories/R语言/编程环境/index.html","d619a9c74e164010ff65494a9f1cf4cf"],["/categories/iPad/index.html","0bbc5aaff5346a65c5ca9534b552ee50"],["/categories/index.html","3715ca1efae1614f4c6ad4988f4df041"],["/categories/中间件/index.html","44db15d4f33b71a46bf5d72c457850e0"],["/categories/前端/Vue/index.html","dd4a1cd5716165d9a233cdd7330114ab"],["/categories/前端/index.html","f37e20ab79a08bcf9de57669ef989458"],["/categories/大数据开发/ElasticSearch/index.html","821917481f36dd689d9206e745333619"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","dc091d5faf88178399e4494e2d582078"],["/categories/大数据开发/HBase/index.html","ca96c10213d7a8e3bc4ccabbf07fcaca"],["/categories/大数据开发/HBase/学习笔记/index.html","8807bb3c6da179bea843a63d339db807"],["/categories/大数据开发/HBase/环境搭建/index.html","0c6f3dc70298a936938306e08161c02e"],["/categories/大数据开发/Hadoop/index.html","573528bff796a1bbf2d1002307895e49"],["/categories/大数据开发/Hadoop/技术/index.html","a373da1308fb707c42c9109aeed3fe61"],["/categories/大数据开发/Hadoop/环境搭建/index.html","12b6c421046b1b07bf6492e6270a0ae6"],["/categories/大数据开发/Redis/index.html","5074d46bab5a7c27c64170ab99cd1896"],["/categories/大数据开发/Redis/技术/index.html","0fef8a50c3d7f12d23ea787d7a4a4995"],["/categories/大数据开发/Redis/环境搭建/index.html","cb159c90e4bd73d6aba140f48cc45eec"],["/categories/大数据开发/Spark/index.html","e66974a7293e547a321db842449401e6"],["/categories/大数据开发/Spark/环境搭建/index.html","cedd37220c3437c600f8ecb5e35b79bc"],["/categories/大数据开发/Zookeeper/index.html","2d276fadeb2eadc74590f9c3be16c589"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","b34b1a148641c8d4bc942d2e610d70d5"],["/categories/大数据开发/index.html","ada05333031428165af3dc2d0634f88f"],["/categories/学校课程/index.html","6f28e5febe97c117d8c16fd5c4d87060"],["/categories/学校课程/计算机操作系统/index.html","3fc098de3ab8d5245832c2d95cac3e7c"],["/categories/操作系统/Linux/index.html","caf4f4f4ff6e7a034ee7a2f0a330a323"],["/categories/操作系统/Mac/index.html","dac3fecd45e5648c4d5e6e8be0db41b3"],["/categories/操作系统/Windows/index.html","d1c2d7c59fd34fc1dc414d291c6714aa"],["/categories/操作系统/index.html","35d4c65f1e4d38a35357d3d31b14f32d"],["/categories/数学建模/index.html","7ee76e13319202512c8eeec348778fd7"],["/categories/数学建模/latex/index.html","b11a0b0cc914ea073a79d3feb0d0b783"],["/categories/数学建模/优化类/index.html","3f57e125695fa5f2ffb214638ffa5c68"],["/categories/数学建模/优化类/现代优化算法/index.html","4e38eb966140c98f626503aa13cc2260"],["/categories/数学建模/优化类/规划类/index.html","8e10c29f89db5487308da47fe719cd35"],["/categories/数学建模/绘图/index.html","ae238ccf3bcdaea2856553a4f8cd342a"],["/categories/数据库/MySQL/index.html","cb524bd63de3ff390949554e4654a36a"],["/categories/数据库/index.html","0775bcbef1a5248e9832f4d0a9c564a1"],["/categories/数据结构和算法/index.html","1f8c2c51a9a413c4aa0bba9b7aeefa71"],["/categories/数据结构和算法/page/2/index.html","5cdb243172bb9f81490b4cf172d5dd14"],["/categories/数据结构和算法/基本原理/bfs/index.html","4faa83a7077705b066c6b434db3a88ca"],["/categories/数据结构和算法/基本原理/dfs/index.html","1e0e017a54f22e1cc126e136ea111ead"],["/categories/数据结构和算法/基本原理/index.html","48e5bcdbcbee4bef3fc4a26ec1d36c41"],["/categories/数据结构和算法/基本原理/前缀和与差分/index.html","1d103dfc3c778a397a9173ea82ab06b2"],["/categories/数据结构和算法/基本原理/动态规划/index.html","6b61e9815f2ca5f34ade2b2c12c2e4a9"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","86435e9385baf7661c132ae44ae6bae7"],["/categories/数据结构和算法/基本原理/图论/index.html","924104263e74785d7a51ce79a8be6bc5"],["/categories/数据结构和算法/基本原理/字符串/index.html","68c854c9626471a8fbd5caad0a8e284f"],["/categories/数据结构和算法/基本原理/排序/index.html","1f7d8cef3e7524781092bd03ac5d70ba"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","37d6afcd34fa5200bc865d30030dc09d"],["/categories/数据结构和算法/基本原理/数论/index.html","0062e989c493d60763b44ba7e2ea98b0"],["/categories/数据结构和算法/基本原理/树论/index.html","5d5fb3b8f487678bfa8bdf59c89158ff"],["/categories/数据结构和算法/基本原理/链表/index.html","b3a555acc95a8da9daad4fbae9034e23"],["/categories/数据结构和算法/算法题/index.html","9c97bcc35699c781f71a695de4ae1f54"],["/categories/数据结构和算法/算法题/二分查找/index.html","83f8c971a8aa637f6532c17967de646d"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","9428e3c216ae2abc87347e0766b0c042"],["/categories/数据结构和算法/算法题/动态规划/index.html","6a19c947bc678dc939095d7d4b2a8d8a"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","7e3ee793762f25aa914d189bf61733f0"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f2353f8dcc1c68a4bc77639ce453a9c4"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","53edbd68358a1b1525093a1e1fc6528d"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","873a506a3a541ba9dd5c90131c9e5c17"],["/categories/数据结构和算法/算法题/数论/index.html","3f3653dfdf70b0f7e0876a94ecd7d39a"],["/categories/数据结构和算法/算法题/栈和队列/index.html","27ac17382588f1cf887175d011ae3d19"],["/categories/数据结构和算法/算法题/树论/index.html","084993c28c6e65f94a6cd1044aaf0843"],["/categories/杂七杂八/index.html","9acf366a0e55377106d3e7cf66399f9b"],["/categories/杂七杂八/博客搭建/index.html","3eb2b797413bac2c2a3c3e7519e60e76"],["/categories/编程工具下载/index.html","21d9a6c165afca4ae2f0d1dd3138ee22"],["/categories/编程环境/index.html","cd3fa1c1d6f701f7c6f3cc740f834b55"],["/categories/编程环境/大数据/index.html","445bf9946d79d7b01ece4f045a498b0c"],["/categories/英语学习/index.html","1ac9d951140cc14a4955425b4fb2befa"],["/categories/英语学习/英语语法/index.html","1fb6930d86bb63670ee0df6166c0620e"],["/comments/index.html","419175dde0d90714d320a509f0582f69"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","968ee375bacb08acc0c6d7463b9914cf"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","6b0763b559271fcd33f6aeb5d8abdb5a"],["/movies/index.html","66f6ef3f1d9fcd6ee774d21bae564236"],["/music/index.html","126f6c190214b37cdede083ecb8f402b"],["/page/2/index.html","4845782175752eb05241f200b526cf29"],["/page/3/index.html","44d580a44edbcf0f2552a4e5d35df8a4"],["/page/4/index.html","fd9296403458c0cb98a8d37848a4e39e"],["/page/5/index.html","ac8b8ebe8d0327794cee00112cf0077c"],["/page/6/index.html","0493bf2ca41e2b9546ed2a43f5119b34"],["/page/7/index.html","2ced8c51de1caa58eb136d3144587fe2"],["/posts/1021360842.html","73e2d41d2863f4cba0fa7e6fd9a1e72e"],["/posts/1120620192.html","4709937127fa5348a4233e1acc8ca940"],["/posts/1137707673.html","c853c3df0d2c4f0ebd8bf0da3d17516c"],["/posts/1141628095.html","da88fcee745bcca920cc7e5441ca09da"],["/posts/1168613674.html","719a915be9061899e651a52405e64782"],["/posts/1219920510.html","8c3446eb87fa6abeb6a156466efe390a"],["/posts/1222166338.html","5df72e0d344c0c7678572d27772db327"],["/posts/1259097482.html","dd79346d0210465357f7467df16e9b69"],["/posts/1271036369.html","7e06aab6c0494c0c6fb555dcfd9b387d"],["/posts/1312847445.html","55cb3ad0abb19146a7f05ee67b5e9976"],["/posts/135355774.html","37835b6ee7b213dfb596f6d6af266d92"],["/posts/1375344716.html","622517aebd011f3a4cd497dbf0c9d05b"],["/posts/1388991698.html","69452b15b2af4bfae916368da554ae0f"],["/posts/1410315814.html","2400c2ba15b9ed9e040adb380432d4c1"],["/posts/1452790229.html","66bd0cf1a7d469d463ae430307124e83"],["/posts/1470079884.html","540fff81858212c0ef5f6310709c50a5"],["/posts/1470079885.html","f630349c565532303f74cc102c7e1987"],["/posts/1470079886.html","46ece0bf418df5c41545260454d015c1"],["/posts/1470079887.html","8655910b6fe2b4b4d57cbad45f4bfcf7"],["/posts/1498536549.html","1c4aa019d500266bff9c4148b7b5a6b5"],["/posts/1539568593.html","d87e5624b0594e40bc7123b8493cab5c"],["/posts/1547067935.html","763500a8a6d59cdfda221a755da80ff8"],["/posts/1557866301.html","7af823f9e6cd28a6ffb3f2f81bddd50d"],["/posts/1571776361.html","029b09bfeeb6e6debfae13ec91775217"],["/posts/1605124548.html","28b2a67680ddcf9ffcb28ad04291b5f7"],["/posts/1633036852.html","7a627e41ec6a0fa291442f1270476b28"],["/posts/1667740714.html","6e31801b65aefc8205ff05c944b3827e"],["/posts/1674202625.html","c938f03eb953ad000d6110ae058d0bb2"],["/posts/1765123828.html","fe1b515a0b905a21f24424a4ec73afb9"],["/posts/1767336200.html","768e416bfb7b45bb60deb881a6033673"],["/posts/1776114197.html","f7b5761fcb6e549c539bf956c330fc63"],["/posts/1817748743.html","a2a8aad4d529a43ff5f87a5c1027944b"],["/posts/1925125395.html","57150367a06e36e4f79f127cfaa4f7d1"],["/posts/1966191251.html","24fad15f97b06a872236831f22258caa"],["/posts/1987617322.html","3648a817d3235727ec3e027a6a06b901"],["/posts/1999788039.html","50465acd57e25f1a701746b3492bcb32"],["/posts/2007534187.html","629b5761794aca511fd65e3141628d4b"],["/posts/2075104059.html","49dc548fd00bfc368db4571dbeaf9468"],["/posts/2087796737.html","b08d0cef76bb3c207ee7aadab4db5b67"],["/posts/2106547339.html","3acf9646fd9013c3d6305d03cfbbeb32"],["/posts/2207806286.html","4628a4a057c421b975e24ede33b22f9b"],["/posts/2225903441.html","806644e944ca52869348882187cc829b"],["/posts/2265610284.html","8133386487c2ed1e0c4915e5e5576d35"],["/posts/2281352001.html","ec595fd76690e9cd85f4575aea1afa79"],["/posts/2364755265.html","8fb4faedd8866bc173ec5f3b0d3b3310"],["/posts/2414116852.html","bb02197663b1738d1595b6af509355dc"],["/posts/2421785022.html","2ba73999b8b5efbf25854abc46e215df"],["/posts/2482902029.html","651ac261f2b8a408753cfb243fd55d1f"],["/posts/2495386210.html","a66e9e5ce2c36939daca3f208615443d"],["/posts/2516528882.html","243b6b80d0c301285d58d3ca3b2a6d2c"],["/posts/2522177458.html","e19ce6e47da7b0459ff8e6bb5ef24310"],["/posts/2526659543.html","842e82232aef8189c9d6a27b537eee48"],["/posts/2529807823.html","d027874a27222a5d9ac0157aad048260"],["/posts/2596601004.html","aa0bded3bc3abdd83381bddfaf1f79cb"],["/posts/2697614349.html","2004a0111b05ea12056e6fd5bf854c09"],["/posts/2742438348.html","a9c17b27c6f2f83b64266964d4437d14"],["/posts/2768249503.html","e23e1edacbaf5eee93e2bdf428b2e9da"],["/posts/2864584994.html","cbcc9f4535efcc26e360fb286e8fa23b"],["/posts/2888309600.html","28b383185610d8a7a1284c42dbcab12c"],["/posts/2891591958.html","d4be0bcf1caee7d2e561b38792a5cbf4"],["/posts/2909934084.html","c6e1142e212ea3a0b94ea2cc4661e94f"],["/posts/2920256992.html","d4c9bc675819257fbd645482a58ed55f"],["/posts/2959474469.html","ad8efa7e53ed2fb1560851e0e827a871"],["/posts/3005926051.html","e03e7bf7c2fd53e068a51ea9b211b58a"],["/posts/309775400.html","9e2a1d2d2532ffa1622ef7f7682d565e"],["/posts/3156194925.html","f749ba291b34143d3ddda6c68c1242a8"],["/posts/3169224211.html","d7d370fbf53e07a1d77eb6fba9b21300"],["/posts/3213899550.html","fbd24a1cc5798784e580385b198a415b"],["/posts/3259212833.html","5959faf2b085a82cda93cb2b1277644c"],["/posts/3265658309.html","21092f019855fc83b0db1cfb74da8a62"],["/posts/3266130344.html","de46dc0076956ceeb1e01329bef131d9"],["/posts/3292663995.html","a2aa0ab5d79618dd03af1265c704bc3b"],["/posts/3297135020.html","10d54ac4bc854bb46af279557eb8c943"],["/posts/3306641566.html","a1afcff0f2093533f7589dd1f62123b8"],["/posts/3312011324.html","57bda0d044c73f8b0bc10e09ce6ac6d7"],["/posts/336911618.html","11487354876ba06d4bb371b5cddd1c95"],["/posts/3402121571.html","930dd93d311d98ff5f4144f10697296f"],["/posts/3405577485.html","919a60cae9504d9517744c1db8e53958"],["/posts/3498516849.html","cc6929d38bf07450b4155667075a56ac"],["/posts/350679531.html","85f3b2edd50136994e37b99593578233"],["/posts/3513711414.html","03732dd7c5348852c55838fcaa88630f"],["/posts/3523095624.html","de2df5527840a7da315f48dd5e86f529"],["/posts/3546711884.html","796b367297e41f73638d962693c83b7d"],["/posts/362397694.html","fc2143717afb4ff315bea4e84cd56607"],["/posts/3731385230.html","d401ecc81f38c514cbbf57480b0b1e65"],["/posts/3772089482.html","63c1c7437a34ed6e7b6712d2d9dda404"],["/posts/386609427.html","b57b309006a89bc032daa97c4ba7dcf2"],["/posts/4044235327.html","4dc32ca42f263edd6ba443a38ece339f"],["/posts/4115971639.html","add51323b7b38ad5800f10364028e45e"],["/posts/4130790367.html","1d31dcb8bc9c020115ab89ee4fee4cff"],["/posts/4131986683.html","af54be1c59a2aa213f526d6cd4eda215"],["/posts/4177218757.html","b15954196e58ee78e1230aebdf4cd15b"],["/posts/4192183953.html","fa4e1353b8d611de671ee1d0925200a7"],["/posts/4223662913.html","6d2d0b81d4b1d72f293635aef2004ae3"],["/posts/4261103898.html","bcf06fabc761f3d6f9a44dc723f0c5fe"],["/posts/4286605504.html","8eec67167686de2a16330dcb95d4eb71"],["/posts/449089913.html","ef7613cad85b13754b76bc9aa2a30073"],["/posts/469711973.html","c2346b9e90be574deba0bf08fa5f7128"],["/posts/482495853.html","48550b515069e99cd81b34d4078c577f"],["/posts/488247922.html","d46615e265a421625cf3257f9e11dfac"],["/posts/517302816.html","ac7c06004d2e21882e8494c6ba361fe7"],["/posts/570165348.html","8bf42774437b014192a0276b285f49b7"],["/posts/595890772.html","c6d55ab9171b980a26fb1cdeed56843b"],["/posts/67485572.html","851885ebe9626d1af6e69ec8d7562b1b"],["/posts/694347442.html","e2aea4acabd7074f02796dd8cf048cec"],["/posts/707384687.html","b496298f20be5610825f5e5fee291971"],["/posts/71180092.html","0290590d5d5d4f465aacae7a8d50c398"],["/posts/716459272.html","ed580d1a08858da195f4afe28c3ed4d0"],["/posts/765481613.html","584eb5ff1bc17ab79433a73506cf4c2e"],["/posts/778231993.html","7a5ceaecae9376f1d58afba763ca2534"],["/posts/795397410.html","7f0fdb557011838c815320b5d42442da"],["/posts/820223701.html","b76ba1d3f45f892c61091c31bd7b9012"],["/posts/830372185.html","587ada570e44334d7b0b0e2f47f747a3"],["/posts/88294277.html","5e79d0275960978041b064fcf356c851"],["/posts/939963535.html","532a174c88eab87aeff66f58ee0d170d"],["/posts/983786067.html","01ca36d9bbc3ece29526dfafc1920012"],["/sw-register.js","b6803dc6078dbe08acc716e4274328e7"],["/tags/C/index.html","acc5f8144e3033f3d7e44a83db360a27"],["/tags/C/page/2/index.html","3f029b5bcfc066e6499f68d84a264f34"],["/tags/C/page/3/index.html","42d58dd6a0e8db937144f13262ffcbe3"],["/tags/C/page/4/index.html","e8ee5bebf71b2c6d50f16ffae14931b5"],["/tags/ETL/index.html","0e5570c70993fbc3ae3c4b65e8333146"],["/tags/ElasticSearch/index.html","75f697e0ef8e31b73b2566dbcedef8f3"],["/tags/GUI/index.html","1fbe601649d4dad0570132fa3549f9a6"],["/tags/HBase/index.html","369d5851b0e306589a2a1e2f0c88a80c"],["/tags/Hadoop/index.html","c50dc53c2f11394630c817bc1e66678d"],["/tags/Hadoop/page/2/index.html","3d4c0b26bb293d2521a497de2f22b53c"],["/tags/Java/index.html","0ee9c845fc191fa70f17b2d604c570e2"],["/tags/Java后端/index.html","e18b9b3213585238573f8ec254403bc5"],["/tags/Java后端/page/2/index.html","103681708abc3a8a7e9c169a194e28a2"],["/tags/Java基础/index.html","6a9640d8b8920e09bae7c1462a8e57fa"],["/tags/Java基础/page/2/index.html","ed3ef82122e01fcf06b919cfd958ce05"],["/tags/Kettle/index.html","9a9b3424606692a2b1a07e878808ae70"],["/tags/Kibana/index.html","87fefb6048608d03ade7667cc57a8fb0"],["/tags/Linux/index.html","9e9e8cf3bda6d7497e136d8443054c74"],["/tags/Linux/page/2/index.html","c0b88e2dfa556051f9b3e6978e916bf1"],["/tags/Linux/page/3/index.html","cfe25f19cb2bb457f252e6c9b4d06114"],["/tags/Mac/index.html","cb4230e86aef9888b670cab95c94d9f4"],["/tags/Mac/page/2/index.html","4c481f233b523a305f9d8397008a13ad"],["/tags/Maven/index.html","d6afe63bdae54b626e709f1273e1ee9e"],["/tags/MySQL/index.html","630e51e67082bad937483bb0c9261ff6"],["/tags/Python/index.html","41b01167f63ce8d7962ddee75352a4bb"],["/tags/Redis/index.html","7226a16f0b9787beabe96b2fd5196a0f"],["/tags/R语言/index.html","8ebe5315cf8bc1f76e45ebb4e5cc7dec"],["/tags/Spark/index.html","c27fdff14fd17c4a028eb6978bfbce61"],["/tags/Ubuntu/index.html","2d3e16686fb4dc2bc00d4f54fbe536e9"],["/tags/Vue/index.html","18f06af64bd78c59483edb91c71e95d6"],["/tags/Windows/index.html","81c376e868ebd0a42ebe5ccb72c79ed3"],["/tags/ZooKeeper/index.html","09615f674f3a07dd63fbc342354e2d55"],["/tags/bfs/index.html","00661edecfb0b4581f4ac27c4d2354d2"],["/tags/dfs/index.html","90e393ca0546627a348f70b9358dcaab"],["/tags/folium/index.html","df3602a76e2ebeca0b368e90e16c76ff"],["/tags/git/index.html","c37436004a1e74d6cb0ed3a682a8d32f"],["/tags/iPad找电子书/index.html","5ca8a289488a68896a151303a4f30c01"],["/tags/index.html","4ac6636b7f4e29d40b23a731060ce93c"],["/tags/latex/index.html","186e8b2d3fd6ef5d6ceb1ee8fdcb1b96"],["/tags/中间件/index.html","c7e1e2ed0f5dd67e254155caa3e0b492"],["/tags/二分查找/index.html","db894e4fa62156fd8a827c881d35c8e7"],["/tags/优化类/index.html","2071ed5f8213df722c3159aedd093029"],["/tags/前端/index.html","284bfe6aa4f7f7d9ae76fc33ec83cfb0"],["/tags/前缀和与差分/index.html","8088cef314e2c54284258c7cd43ef1c5"],["/tags/动态规划/index.html","7867b0adcbd6327dbbeb37db3e0ab3dd"],["/tags/动态规划/page/2/index.html","4e96bc8326b762ffb7c75789b70468cd"],["/tags/博客搭建/index.html","1b8111f2aedd1fdee61e5200ad9719e5"],["/tags/图论/index.html","e2fe5bc06c313600973510156db82e27"],["/tags/大数据/index.html","84a94038651b1eaf2ed966c4a54f0f41"],["/tags/大数据/page/2/index.html","18494feb8eb9a24c04ba2da7cefeb88a"],["/tags/排序/index.html","ffe6b6d91bbc8ec614a1511ef39d5b75"],["/tags/操作系统/index.html","7212d1b67fdbc83ddb9dcee16eeee485"],["/tags/数学建模/index.html","c5cb2457b9ad01e3a8e402cae99046fe"],["/tags/数据库/index.html","454bf245a032aab3124921b3a5dc74e3"],["/tags/数据结构和算法/index.html","812866a6d7d50b5a6ad51e5e4990e353"],["/tags/数据结构和算法/page/2/index.html","9c340e73e5c8a9524dc84aa366ba8041"],["/tags/数据结构和算法/page/3/index.html","3c9684cf20a86f865cf155b616c728de"],["/tags/数据结构和算法/page/4/index.html","e73ed5524816b41a0836171d62fbeabd"],["/tags/数据结构和算法/page/5/index.html","aefc866ecd175c5601f6f0dbca103897"],["/tags/数组和字符串/index.html","8e0328b4fc334c909f26b7958e7960dd"],["/tags/数论/index.html","9a76da332de3bdfc941ab8867a6be1e4"],["/tags/枚举类/index.html","c3f39c769764bbfa368d2f05bd44f526"],["/tags/栈和队列/index.html","2199eba79d1da61607a64098dca7cfa1"],["/tags/树论/index.html","63668c11458ae8d474115f535229032e"],["/tags/测试/index.html","2b3c5abcb24912eab70aad7b4b947a96"],["/tags/环境/index.html","38e2fb8af71ee0830b14dedaf3b3136e"],["/tags/环境变量/index.html","83276320a55e6c6611c71ce0c8cadc9d"],["/tags/绘图/index.html","e25803e9c3d0c7570ac2d37a94860120"],["/tags/编程工具/index.html","acf266044e2f341b12fe86a183c4c1e9"],["/tags/编程环境/index.html","dd2e4ab9397f5ef708a344e184b47aa9"],["/tags/网络编程/index.html","9014dfc0d1490b6c01e3058e4244b77d"],["/tags/英语语法/index.html","fc09c535aa8acd29a36de16da82007e0"],["/tags/计算机操作系统/index.html","c5f85af682d5938bf3577ad554a5aa76"],["/tags/论文/index.html","5ce86732bb9274d793bfc3546f4a5787"],["/tags/资源下载/index.html","5d83d7f0f0c199b40a1bb0813e5999f6"],["/tags/链表/index.html","8be85fa753c383872264640840938c89"],["/tags/集合/index.html","c1ab02657630eb9d9bed4735937b7306"],["/tags/集群/index.html","29330c9b5b61c18ab222bf061a3b485d"]];
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
