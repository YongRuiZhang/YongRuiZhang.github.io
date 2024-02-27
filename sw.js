/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","ddb35cda897a02ae86860ed70edc6748"],["/about/index.html","599438f318f91646df1d53d78d6e68e8"],["/archives/2023/01/index.html","d131cffbe839f2a202fb468e98700b9d"],["/archives/2023/02/index.html","f9559bc4a43d7a629cd3b055207d7ae7"],["/archives/2023/02/page/2/index.html","683bcb12216427f83ff532f14c6e6587"],["/archives/2023/03/index.html","b3cad8200a013ae72bc0adf75c561ba0"],["/archives/2023/05/index.html","ecd0a0458dbc5b950c02eedc4b553377"],["/archives/2023/06/index.html","efa82ce4867a3a292c02a842d7b14963"],["/archives/2023/09/index.html","9212e6e730ae42c0f7334d081cb11bbd"],["/archives/2023/11/index.html","9b30278db1d45a93aea088031f9ab114"],["/archives/2023/12/index.html","5d6c9d92afe78a23ad3848f8b8bb6bd7"],["/archives/2023/index.html","75f88172990c8141d75008a748508b86"],["/archives/2023/page/2/index.html","f045cb658de21eaf95bb2112fbb57826"],["/archives/2023/page/3/index.html","fc2db977043e28150b75da83c8b038d4"],["/archives/2023/page/4/index.html","d72b864418fe9c355a5236e6f43a12f5"],["/archives/2024/02/index.html","aa97f730679b404cb066283e462fd00a"],["/archives/2024/index.html","b8a4d8dae7a67542f65cffa00f723062"],["/archives/index.html","1d6bf5490ba2f26156d9f64d8ca16a31"],["/archives/page/2/index.html","aeb0ae4888fe464e05293cc9b2afbc14"],["/archives/page/3/index.html","380ab75e4b76ca7d798d9ef286dff46f"],["/archives/page/4/index.html","dd2eec7859f6d6dba529bdae9203ef6d"],["/baidu_verify_codeva-qQP2iZOMLX.html","b8335a1aeab3e00af4cfd2a15460ca93"],["/categories/Java/index.html","6163f137c725554cb00a29db0e5eedf4"],["/categories/Java/后端/index.html","15e9033886861b8b0a3065d0cf759d30"],["/categories/Java/基础/index.html","90d91027aedf4b93993c9754bf551c2a"],["/categories/Java/基础/集合/index.html","60a50bcb342f0b7ff94aa1e47f4e4ad8"],["/categories/Python/index.html","41869447e8996eeddbab6f3d4e7ffc52"],["/categories/Python/编程环境/index.html","844cd47ae4f3ef56df32a27f0dd7763b"],["/categories/R语言/index.html","a0e5f7cd8341a3bb9066a6e2a210403e"],["/categories/R语言/编程环境/index.html","3619286ae7b57b3681e462f17159009b"],["/categories/iPad/index.html","fbaf9dd62e7069f27e61f4402d79dcc2"],["/categories/index.html","5bd9eb7bc5866e4bd570906a3c04663b"],["/categories/中间件/index.html","18d9a1f144f0ab5cb012b78645d54ac0"],["/categories/前端/Vue/index.html","a1c2ca4a206baab8dfaffc8c74b28740"],["/categories/前端/index.html","44eaa4edde14a604d8b5c401c70046af"],["/categories/大数据开发/ElasticSearch/index.html","b3e90e8b560d8189c6c46695dc226e38"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","2243421ab5a435e41c675636348213b3"],["/categories/大数据开发/HBase/index.html","0387f4e85d9354a71137ed0660bc3661"],["/categories/大数据开发/HBase/学习笔记/index.html","d2d1ddc90476e9d068480e76cbba967a"],["/categories/大数据开发/HBase/环境搭建/index.html","e24a3d422dedd765f5553349d4e2c270"],["/categories/大数据开发/Hadoop/index.html","927b2cdf7e04b18d64f39249555e98ec"],["/categories/大数据开发/Hadoop/技术/index.html","8d4ec62536e5fe20b348dd2069025f2b"],["/categories/大数据开发/Hadoop/环境搭建/index.html","744e373bd6f33ef8fd6566c865e0ddbe"],["/categories/大数据开发/Redis/index.html","e11384b4f3514698548f09517f009860"],["/categories/大数据开发/Redis/技术/index.html","171ddab759d92a381cbf408f8a663065"],["/categories/大数据开发/Redis/环境搭建/index.html","f51fd5e5b168ba680bd3e2fab025b06f"],["/categories/大数据开发/Spark/index.html","a4b8f68c28288e2d8add7931c77ce559"],["/categories/大数据开发/Spark/环境搭建/index.html","a475c78aae7da79480a8ca277b0b4db5"],["/categories/大数据开发/Zookeeper/index.html","881d2556f7b72cea541cac98db1a31e0"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","6b1eea273b498b277ceb78c98044b123"],["/categories/大数据开发/index.html","257c4ccd8f2803b9e9dea43c8a5972a9"],["/categories/学校课程/index.html","99785f6db0d433d9bf949ba7cfa79cca"],["/categories/学校课程/计算机操作系统/index.html","f0549ca32e5e90d071245ddb17fb9650"],["/categories/操作系统/Linux/index.html","5d93131858adf7c494569221711b830e"],["/categories/操作系统/Mac/index.html","232567f220283b7fcfe76fcb9255545d"],["/categories/操作系统/Windows/index.html","b176f8787642d98d77ea131c6387f656"],["/categories/操作系统/index.html","f4be9042790d0183293bcff6b0a151a8"],["/categories/数学建模/index.html","39c1643aae2a0e91e4d3953e94591c2a"],["/categories/数学建模/latex/index.html","7cb13a5041632b86beea5c7c136896f6"],["/categories/数学建模/优化类/index.html","5e0af4f32f367f2968378b4a2f797827"],["/categories/数学建模/优化类/现代优化算法/index.html","9a091d9e6901c24ec915cc267d3a3d88"],["/categories/数学建模/优化类/规划类/index.html","5c6798cbf1039b7740496b28d27b499f"],["/categories/数学建模/绘图/index.html","6ede6e65e69d5f86e4f7e7e94da71741"],["/categories/数据库/MySQL/index.html","0509b7c12443f01648b60abb3fa22140"],["/categories/数据库/index.html","1e30e5124f5d9023f77f9b6a159f8aa8"],["/categories/数据结构和算法/index.html","57018eed433c4f5be7920af5f25cfd78"],["/categories/数据结构和算法/page/2/index.html","4e816c37312972ca1badf8f4ba614f33"],["/categories/数据结构和算法/基本原理/bfs/index.html","a830c8f554a965c38e4eb1b51f3959f9"],["/categories/数据结构和算法/基本原理/dfs/index.html","50921eb8cea7f01c37949fd7f2c21068"],["/categories/数据结构和算法/基本原理/index.html","d63e60ff50136dda9b7e0602c97470a3"],["/categories/数据结构和算法/基本原理/动态规划/index.html","935d7916b432b9352d9d90866d563ab6"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","9be760bc233c7d49cc0e33276d2cc6ec"],["/categories/数据结构和算法/基本原理/图论/index.html","2170a6fc86613cbd884462323ab8351c"],["/categories/数据结构和算法/基本原理/字符串/index.html","f80ca13019b4e7313e11b97f298b8025"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","0a2d6ab4ec5340f87b24eaef8664d00a"],["/categories/数据结构和算法/基本原理/数论/index.html","2544fb8effa69392df6c84c913cafe6d"],["/categories/数据结构和算法/基本原理/树论/index.html","d1d9cf466b62bdedd4007a9be4162577"],["/categories/数据结构和算法/基本原理/链表/index.html","6d786bffe6fbfe41e4c73d2ec4f009c0"],["/categories/数据结构和算法/算法题/index.html","7c403f54b815917c19d1de8906065ac5"],["/categories/数据结构和算法/算法题/二分查找/index.html","2e25a964966b6abf3e1ed3c730fa4394"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","95ad0809980e6ab47a539e93554462d2"],["/categories/数据结构和算法/算法题/动态规划/index.html","06324d85f92b9f9e9a6978d8231ffae1"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","bc2b9140e0ef3e66a82ea3a28d2523c4"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","614a41d733a23b830c24f72b27c8ce4f"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","2d884584c0bc18ac2716ccd66b86a07e"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","80ee081ee3fcbd8f2961886d17404b87"],["/categories/数据结构和算法/算法题/数论/index.html","ee05e62332372ef0037263957439655b"],["/categories/数据结构和算法/算法题/栈和队列/index.html","5aa05c1680f0f6601a1df68702fb8f0f"],["/categories/数据结构和算法/算法题/树论/index.html","6c7110017be78e62ac4b77d7ca3fbbf9"],["/categories/杂七杂八/index.html","282337b1dadc8c6aa371d195d956ae4f"],["/categories/杂七杂八/博客搭建/index.html","80b178c77112c11082d6662af985d8eb"],["/categories/编程工具下载/index.html","8cec49721249604836efe255e2197c50"],["/categories/编程环境/index.html","bc7f62716ae261fae814709bde2a751e"],["/categories/编程环境/大数据/index.html","ed58eb089a93af5346e1294ab5b5d276"],["/categories/英语学习/index.html","599a9a011cb1a4d2f1e95bc71203552d"],["/categories/英语学习/英语语法/index.html","2b67c08a775cae4905c22d44762deb2d"],["/comments/index.html","207d37895ee21f7deb9512e2ac08de4f"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","ca31c8a3ca6fe90426ff1c5abab2cb30"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","820965fec54250f1fbd922519c818bbc"],["/movies/index.html","8522adf92d68f2df17a12c794b0ec2ae"],["/music/index.html","32cc48025955efdbd3082a41118d13f1"],["/page/2/index.html","46132516ea6d1b7fad2199a105ef3767"],["/page/3/index.html","ead690d021d9204c71b9748b2e6a9c84"],["/page/4/index.html","aabecd480beba8702609640072af8fde"],["/page/5/index.html","da47d8cae57a39463291ffd0d6090cc1"],["/page/6/index.html","2f4ae123bc0798d782f4285a0c9502d6"],["/posts/1021360842.html","a7f32f1ec0e4d2e38de9688cfc72deb4"],["/posts/1120620192.html","6b557c1aefa74a3cadba0b55da2adb26"],["/posts/1141628095.html","6ff6d392b09abc5221f69a7376c560d7"],["/posts/1168613674.html","a0378ac0fefbcc5ba7b442af77528534"],["/posts/1219920510.html","59b50182ec4b4f6b7bd0247588f9d308"],["/posts/1222166338.html","3401f066b0374fab7bf81c6a54e75fc9"],["/posts/1259097482.html","1c7dc2b212c2c6f3dd6de706383dfe21"],["/posts/1271036369.html","be0b4d514ebf1f0d3334be898fafa0ad"],["/posts/1312847445.html","0f9755bd990c6be5090a04350e4b34a1"],["/posts/135355774.html","32edbf0180bd92811205cf7bfe020f19"],["/posts/1375344716.html","81da0967ec701248bec8414df0e6a5f3"],["/posts/1388991698.html","f5682002251f51d22210f1dd5f32f58c"],["/posts/1410315814.html","ef0ab8dd5cdae1702bbcb965161a17c1"],["/posts/1452790229.html","c87543ab52289cd6a4a41ab94bf9b8be"],["/posts/1470079884.html","e0c638db656e6886d405a67bbd105e9b"],["/posts/1470079885.html","2cb933caa83d46efbbe3f5dceeb92d60"],["/posts/1470079886.html","676686777ecd69ec107c3bce4ddbcb5a"],["/posts/1470079887.html","4c28f6962e34fab091bcee05bf02b1c8"],["/posts/1498536549.html","8209c51ec7d812da9891c2b49e121489"],["/posts/1539568593.html","02746106748dfedbc823901a5a3839ab"],["/posts/1547067935.html","301de0684a1956f3327adc579865ead5"],["/posts/1557866301.html","7b2c81e26411bc2af56ef14e16fd39bc"],["/posts/1571776361.html","ef4a16c7a716b20469dad2282ff39c1f"],["/posts/1605124548.html","95c81c4f96837b2f497f3a613dce844a"],["/posts/1633036852.html","ebf120efead323f3747a5359dce16509"],["/posts/1674202625.html","c87175e4d86f636aea486f0d7c87636a"],["/posts/1765123828.html","0753ff3669c206c385193d4c0d53b915"],["/posts/1767336200.html","6ece6a2d5b680dcef8769d57ffc98cab"],["/posts/1776114197.html","9ab3cbee3d8b742001f9db89dc0b7111"],["/posts/1817748743.html","9543c9dab0a9db91476830e7ee3a0737"],["/posts/1925125395.html","bc49aff4296b4bec3131b558de599af7"],["/posts/1966191251.html","bb7492a2faaf463cd0c6e0974fa4c957"],["/posts/1987617322.html","48ab4e0460b5f534492792699ca87d78"],["/posts/1999788039.html","3dc24f486350317d9bcd1747ba6844ee"],["/posts/2075104059.html","a4f74a7a376b3bb437fe84c42e4c7aa7"],["/posts/2087796737.html","17fd2581012075257ead265cb6168cf2"],["/posts/2106547339.html","be01a0389d32b3ec6099c345b758fc2b"],["/posts/2207806286.html","189a57775953af438811632898a16017"],["/posts/2225903441.html","7573d9a66211ab6542f588d02328032d"],["/posts/2265610284.html","91354b55e9b86d0622c7651d8d2ba733"],["/posts/2281352001.html","28b922be5d69d1eaf1cec1053c390daa"],["/posts/2364755265.html","03303e96e136b5cf6702ee7a0ed2869d"],["/posts/2414116852.html","df6d4fdb104e6963b2cdbe65268de923"],["/posts/2421785022.html","7b8c884b44bac607d73eb9a2a8fee926"],["/posts/2482902029.html","6139735a2749844423973a371bf8e54e"],["/posts/2495386210.html","51f3bb81529da9ea4f06f8a82381a7dd"],["/posts/2516528882.html","90aa49de9e5d2b2b2270a0e1a0a0499c"],["/posts/2526659543.html","43b974bf79160f9eb734b6cd9354cca0"],["/posts/2529807823.html","2958758e59c6df0c1493ccc4bfe2b30a"],["/posts/2596601004.html","e9556a9316816231d4603fd95411edb2"],["/posts/2697614349.html","930827897fb410c967090b5ca48d941f"],["/posts/2742438348.html","1b3cda90c045430c8944c712f3e9ea41"],["/posts/2768249503.html","486dddc85f525f83dd7717f3b72e1c37"],["/posts/2864584994.html","ed688aa655fbf18956e98a8f3294210b"],["/posts/2888309600.html","14ad34c6dcce8ec4ed8cfa5a7316572e"],["/posts/2891591958.html","467892431357369e6da06d2bff996226"],["/posts/2909934084.html","2bcc25807197c07fb6a12c1aa9b5f1f1"],["/posts/2920256992.html","2ba407252edf4d74d975f040a5bd9b5d"],["/posts/2959474469.html","e7156f7d44b0dfc662977bd602460a07"],["/posts/3005926051.html","50c41dbfb3e566e1568073ed6645a140"],["/posts/309775400.html","a2719d07a5c7ca3c946927c6eb67d124"],["/posts/3156194925.html","9c86f5d1d80b8733b4de39a1e9ed86d3"],["/posts/3169224211.html","e1fba2351c1b261f7ffacd5c25024fbb"],["/posts/3213899550.html","5a6f43e2e9f93d6a0583fc15489aef2f"],["/posts/3259212833.html","fc6881436bf7a0656924907dc283e55f"],["/posts/3266130344.html","82e2b99de8c714ae738aeef44ffa8681"],["/posts/3292663995.html","272eb135d88ac9388d605762ec9f300e"],["/posts/3297135020.html","7bb76375ee9e3e090219dc6be07b5364"],["/posts/3306641566.html","58f2be042da2d38c6d68379a74e8deab"],["/posts/3312011324.html","52e22faf971a44e4839258bacda8efbc"],["/posts/336911618.html","24aba10f0859eddd0ba3ab2565d5f342"],["/posts/3402121571.html","83a2ce040d5da949f92be1744c054eb0"],["/posts/3405577485.html","37b44ec17ebf362677b433af8594b1fd"],["/posts/3498516849.html","3cbc5445edcf1e7598c97155f0f9a97f"],["/posts/3513711414.html","caee718e21bcedb73f1efd570839b6a6"],["/posts/3523095624.html","e5dd89d630f35053cac507b5d53a688a"],["/posts/3546711884.html","86f62ee38b7e5115a29ef0d6c3dd800a"],["/posts/3731385230.html","f5fb0413dd24a1af49f18d3c41ec98ca"],["/posts/3772089482.html","def4bca295dbe243a912d7a5a6eeb1cb"],["/posts/386609427.html","85567bda7ebf28fc053a9a0ba142ef62"],["/posts/4044235327.html","fae48e2a5f2f57a5ee90e9fbf116008e"],["/posts/4115971639.html","712758a2b17fae0c925fb5877607ad85"],["/posts/4130790367.html","dbf726ae1e4c48c219bb69d5c2f59e28"],["/posts/4131986683.html","9b6220ae32a7b5a2b7f267386c4b1b31"],["/posts/4177218757.html","36ca8bfea85004a5b8515e4663f8dbf1"],["/posts/4192183953.html","d9bfa87e00818fe5da83da9ee93ae242"],["/posts/4223662913.html","54507ded8dc78364dcd3fa0a968f4c4c"],["/posts/4261103898.html","185321bdeb632252e59c5a1a7a98b6ac"],["/posts/469711973.html","7794695d2a3098a748303e21ba8851f5"],["/posts/482495853.html","59cce32224d128eb4f239bfce1f041ef"],["/posts/488247922.html","b7ea5db3a55188366a5ad54fc155c66e"],["/posts/517302816.html","fc5e3f05fec40a8c96d97ab30b82611e"],["/posts/570165348.html","6255d1ab12d716093101160fa110bd48"],["/posts/595890772.html","f6057e2d74c8512b855d33c445846e24"],["/posts/67485572.html","fac63e0c16eee4d5721b381be6a3c1e0"],["/posts/694347442.html","8f3407e71f3e88cfc74d740e47ee457d"],["/posts/707384687.html","450d64a90c4b533961710b3d32d5bf60"],["/posts/71180092.html","d2ba0a8cdaeb5584b99fe023f974359a"],["/posts/716459272.html","e818ccbf562e5c738fb9337e2400d8bc"],["/posts/765481613.html","79b1fa3dc7be11344a963a6ed9dbea57"],["/posts/778231993.html","15c024605019ce9157dacdb43b8c3cb8"],["/posts/795397410.html","533190a725a16c271ff5378c2889a230"],["/posts/820223701.html","ae6e13d47feeb02d2dff95064405362f"],["/posts/830372185.html","93bbb5e759b66d67d9f8d0322bcfbd7a"],["/posts/88294277.html","88b66f65e9f765260185445356c6a387"],["/posts/939963535.html","dbbbd0c5212cf24c36f7dbe2052e7782"],["/posts/983786067.html","cb2a00d22e4e10c22050c944ff9cb56e"],["/sw-register.js","cc30f5be5401eedfccb17f6725fed549"],["/tags/C/index.html","6ccd00e370cbda8373520b5fd7f5902a"],["/tags/C/page/2/index.html","e87e43bbc385fa4a2ad67d6a7c807934"],["/tags/C/page/3/index.html","57d659a2f84e9a6e0c73f6126e7d499b"],["/tags/C/page/4/index.html","b3ec9c04dd2a99c0fbb04f871f4ff12f"],["/tags/ETL/index.html","e77a4ddebc3eab45001d4256b613cd6d"],["/tags/ElasticSearch/index.html","5b3d3935d27f893619a5c98c31718866"],["/tags/GUI/index.html","ed3197b1cbb6a55a35b28edfd5386cb6"],["/tags/HBase/index.html","1b38b4fd8fea146f11f9e6b17b1bf2c7"],["/tags/Hadoop/index.html","22717ca96f292177650c061b29ff502e"],["/tags/Hadoop/page/2/index.html","85cae87ed123e821a5d8749a389698fb"],["/tags/Java/index.html","18aaba5a09e604a76a401c2f2df025a1"],["/tags/Java后端/index.html","8eb54042e98b97ee6becdaa3c5d5c11e"],["/tags/Java后端/page/2/index.html","7a25bb583eda6c17937cf4e9a9372c70"],["/tags/Java基础/index.html","12508c28ae399bb2bd5c07b265284bf7"],["/tags/Java基础/page/2/index.html","20a7b63f584104f0d12b51386b808e3b"],["/tags/Kettle/index.html","5ff250bb1740e61d4ba2fbe83efc2441"],["/tags/Kibana/index.html","05aa59d0bbd32cdbc7953fd7f2aec093"],["/tags/Linux/index.html","3284dda843230f736c86b698336441fd"],["/tags/Linux/page/2/index.html","6ebc730e8fa17fe14060fd95667a0dc6"],["/tags/Linux/page/3/index.html","424d372461f1dc6c790dab2f8d980df7"],["/tags/Mac/index.html","58c831c7ca06d31697aa2fd46257196f"],["/tags/Mac/page/2/index.html","0cea37d404038d980b5c55750b825a2a"],["/tags/Maven/index.html","796dee5997e27ad3c6622ca71ccc086c"],["/tags/MySQL/index.html","8bcb446b76ab09bc2e18862e2e61249f"],["/tags/Python/index.html","dfa520aa729616f51489fd40a07c7ac1"],["/tags/Redis/index.html","259e85808abc336ab9e3288b8d03a91c"],["/tags/R语言/index.html","127ee1974f068ef2d5b77eb672502fee"],["/tags/Spark/index.html","94d4faa4a20d1903b203b12b3432b058"],["/tags/Ubuntu/index.html","a6fc0fa712aa440524252ca7bc8b6377"],["/tags/Vue/index.html","f80902fa54c1ae108bd93c8b49fb0b1c"],["/tags/Windows/index.html","ac11af7e7d81911cdf5d65e3b98ad4be"],["/tags/ZooKeeper/index.html","a147687522901e093c986725db475db8"],["/tags/bfs/index.html","f4d6f7d32e760db51d42c85e1ea69819"],["/tags/dfs/index.html","a1496c1ae7f11617f8a6e8d85b4bfa6c"],["/tags/folium/index.html","3b8166c10367436109b6a063c7c5ec93"],["/tags/git/index.html","3603fdb64fc0cd99449f74872dccdc64"],["/tags/iPad找电子书/index.html","192a1bfc9b0c0d7ab4df2d883c43bff8"],["/tags/index.html","34d5baf7a3217f94812d198b11d8e50c"],["/tags/latex/index.html","a57f6de14baa3843c0e91c7cf1f89f0f"],["/tags/中间件/index.html","1fe6c8f371368d8a62ec3d6b1cf51f61"],["/tags/二分查找/index.html","cdb50a18420e8be0d895646c17cc0570"],["/tags/优化类/index.html","4ce725b5b26ed7f55a3bfa645734f166"],["/tags/前端/index.html","a2cd81724e783745eb5788541c9a93aa"],["/tags/前缀和与差分/index.html","a9758cbf5f2f0e5429c9fadea1d8108e"],["/tags/动态规划/index.html","df33422e7494d681a6cee3486bd954d5"],["/tags/动态规划/page/2/index.html","70fb0df81439d296e6cf47f729828a2b"],["/tags/博客搭建/index.html","52e2fb2732f7d97f7141b0f91ba3df85"],["/tags/图论/index.html","3325e5291b729f5bdfbabf9b78d643eb"],["/tags/大数据/index.html","0045bc6004e62a941ae8110292bda9e0"],["/tags/大数据/page/2/index.html","7b32c667bf6233c50644b765d68c44e1"],["/tags/操作系统/index.html","a7412ca15394deff932dcc587cd67742"],["/tags/数学建模/index.html","da681888356400b0c40a03f9363943cf"],["/tags/数据库/index.html","29d8c138e0c69fa8cb3597b415441a20"],["/tags/数据结构和算法/index.html","d5a5b70342b806f9fa2d4636b20fff79"],["/tags/数据结构和算法/page/2/index.html","435c7f392f983b0a2c6152412a4c2413"],["/tags/数据结构和算法/page/3/index.html","eef2fc4f3bcc517770339f785a950580"],["/tags/数据结构和算法/page/4/index.html","ba491cd899f8ecdc4af8a180c27530df"],["/tags/数组和字符串/index.html","bfe3967c0287d280067836fa3c1fe0a8"],["/tags/数论/index.html","4b96e160b9bed448577cf8a3b6f95556"],["/tags/枚举类/index.html","a877d0fac79375e094d368fae921c7b4"],["/tags/栈和队列/index.html","e2fb289615af07d2939e2d6f7c68de6b"],["/tags/树论/index.html","ecd2ad6258fdfc289b6fd48ee09893fa"],["/tags/测试/index.html","9a0affaeb75ec0e5cf63967af809faac"],["/tags/环境/index.html","88eb641e86faed7e67b8a39afaa05ed1"],["/tags/环境变量/index.html","ce58e360cef0fecd10d0c0ad22736923"],["/tags/绘图/index.html","b3798619abf3433bdbcc8bfeaac5d80f"],["/tags/编程工具/index.html","89343889af842c6eab200e62fac1b10b"],["/tags/编程环境/index.html","e46354a1ca2eb93ee6f868dc547a60d2"],["/tags/网络编程/index.html","6b0e15996eda0c82f7a8c40d8d1644c4"],["/tags/英语语法/index.html","7d32045e8d299765050017b957a2d819"],["/tags/计算机操作系统/index.html","fd632b00942abb38f447c765e7de968a"],["/tags/论文/index.html","caead9b2dfa4e528839c688811c7ea8a"],["/tags/资源下载/index.html","e21ff1d2e0ac52b33c6a83431682558f"],["/tags/链表/index.html","73eef663c91080ab4c9ab6eea02b8f31"],["/tags/集合/index.html","7cb663d02445de664174666806a8c0a5"],["/tags/集群/index.html","7b2aad5a38562cddb372cd162653bb56"]];
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
