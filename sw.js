/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","07ac5adf63a332fe2bcf9929c489ff29"],["/about/index.html","4429a46a844108359d9e8c3a786373ee"],["/archives/2023/01/index.html","0c1ddbc36e489c6e8e3c02bc97bc0c3e"],["/archives/2023/02/index.html","2606bcf34c1dc6b595aad4af0fdd5aa8"],["/archives/2023/02/page/2/index.html","f241155589d563d20270978cc676c850"],["/archives/2023/03/index.html","a6029966909e1b7f2f56779e57be4969"],["/archives/2023/index.html","978961d706429fd6e2f81cd832539816"],["/archives/2023/page/2/index.html","9181fe9b4258d622c818752993fb4154"],["/archives/2023/page/3/index.html","c026664dc72ca6b8524bfd36c0df06a4"],["/archives/2023/page/4/index.html","7053541b8bc38cf31feaaab93484295a"],["/archives/index.html","82c09c7e95d0deb3d02357d69bc3c8fb"],["/archives/page/2/index.html","1a0107f34c36bc0df28a6765a257bee1"],["/archives/page/3/index.html","02ad15c4fdf4e5c9a4089389f8cb42f0"],["/archives/page/4/index.html","d506e18a97d1a8d0628767cd21d7c50a"],["/categories/Java/index.html","91379dd85f37954d50e4b3d42faa9c44"],["/categories/Java/后端/index.html","508b87131d0c5f0c070a90ab06158844"],["/categories/Java/基础/index.html","066a92cf76f7db1544b857892ffd358f"],["/categories/Java/基础/集合/index.html","15eb3257a7b13c80a71ee2c9f2e3c588"],["/categories/Python/index.html","769730268eab00b2a788af818b66c60b"],["/categories/Python/编程环境/index.html","2720781eafd0b9a0e2b5019784a2d429"],["/categories/R语言/index.html","e35a5686fd6d540c7c755d65eafaab26"],["/categories/R语言/编程环境/index.html","9001a95e310c70197c8803a80917b579"],["/categories/index.html","c300a8ef575fd239684d7041d737376c"],["/categories/大数据开发/ElasticSearch/index.html","5822ac4f480077e353e91205732d9a16"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","7b917a5f7f86de1a524512c22e8803de"],["/categories/大数据开发/HBase/index.html","9aa2015f1d5f48ebb41d4cf4d184d01d"],["/categories/大数据开发/HBase/学习笔记/index.html","0b5a0d464d457f1e278dea609af20007"],["/categories/大数据开发/HBase/环境搭建/index.html","24545f4e9d7a1b8a4dc711f46f1375eb"],["/categories/大数据开发/Hadoop/index.html","21f1d80aeb1faca66d5e3cee2eb447e8"],["/categories/大数据开发/Hadoop/环境搭建/index.html","c1ef078f27900451e49d68b6798def30"],["/categories/大数据开发/Zookeeper/index.html","05408689f429ba04bd4a503a68cafbe6"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","4a94d25d2c1151993198645e393b3abe"],["/categories/大数据开发/index.html","f1277991ec0cb4c2351a23891d39b2ea"],["/categories/操作系统/Linux/index.html","c40bb808a89135386c70f6ead741228b"],["/categories/操作系统/Mac/index.html","a523aec9a21d169d26ddf612cd743f39"],["/categories/操作系统/Windows/index.html","7e4bac12b2d32001ad51169a9e4df38e"],["/categories/操作系统/index.html","8ed4c351cc0055ed269c28271ed18c24"],["/categories/数学建模/index.html","bf84fc8df52cf9fb54466d4baf262820"],["/categories/数学建模/latex/index.html","efa373daffa9195e30fe8b04fad727f7"],["/categories/数学建模/优化类/index.html","97f725aef252de72f1d34812aeea8cad"],["/categories/数学建模/优化类/现代优化算法/index.html","c1a55af70cc199fddb1bfa7e2ba1eda4"],["/categories/数学建模/优化类/规划类/index.html","73dba48133a6cb6702cb6e82b468eb55"],["/categories/数学建模/绘图/index.html","9abd8df592744b3924821fb6faf45c62"],["/categories/数据库/MySQL/index.html","1fc58cd991450e3c7f3ea8819bccb0d7"],["/categories/数据库/index.html","b40d3201c9a7d04acac67a86cdc5a60b"],["/categories/数据结构和算法/index.html","672877b0b6af695e368051e4eacad171"],["/categories/数据结构和算法/page/2/index.html","43b952a5cc8b894a8b6695a10dc9dac7"],["/categories/数据结构和算法/基本原理/bfs/index.html","fa18c4ca965e46f9227a13abafe8bdd2"],["/categories/数据结构和算法/基本原理/dfs/index.html","62228a98c2f96ea81c4f588ba6aa5bda"],["/categories/数据结构和算法/基本原理/index.html","84e434e4c74394173acb3939cb37f373"],["/categories/数据结构和算法/基本原理/动态规划/index.html","cbd5fe5ee3b145831a7cd06bb65a65ad"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","d88d2c3820bcf473f29f5e9e743ffc8e"],["/categories/数据结构和算法/基本原理/图论/index.html","b42d6b15da55fb3b887975dd8f43e2c5"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","9a6e30d753940ba73dea6ed830a09cc2"],["/categories/数据结构和算法/基本原理/数论/index.html","cd14358be1b9799d99ac3f4e854d8031"],["/categories/数据结构和算法/基本原理/树论/index.html","859201ad1a346783681a912129c04296"],["/categories/数据结构和算法/基本原理/链表/index.html","f891a200e0df5d20d64b4142b71dab19"],["/categories/数据结构和算法/算法题/index.html","2685b674a7a8db264ff0b989b5fc5960"],["/categories/数据结构和算法/算法题/二分查找/index.html","af95330ae5dd8dc57cfaeeab4fb92285"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","9ad32130fb959216744b402d973af5c0"],["/categories/数据结构和算法/算法题/动态规划/index.html","bc2dc54883a5e7334525f113234f7cb4"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","aceda2f6741aa08bbf926b9d57a3ff75"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","f617e32db6d8ccade41bf9ea80f8a096"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9f9a7d720fd406bd4021f9efcd873951"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","0189f153e84ff6258afe6ec82abf968e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","c500e4eb2d47d26e403aac822ea5d2c7"],["/categories/数据结构和算法/算法题/树论/index.html","e25408b811b573f9242acdb07181abbd"],["/categories/杂七杂八/index.html","3eb9163eda1f25d0ef44546c5ba79032"],["/categories/杂七杂八/博客搭建/index.html","21f1252c2dcdc2cabdb575d90a2c6995"],["/categories/编程环境/index.html","8604d620df2e5b2e90fc4e43d461256e"],["/categories/英语学习/index.html","b1279b34e11809addfa6c2fb3d0e80a4"],["/categories/英语学习/英语语法/index.html","2f622f1efe2075d7fda595cfd5629c9a"],["/comments/index.html","301e235954ce5b6bbf1ad01b90b0e29c"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","cbe1e11c1eba8e70c4c1095efaa7438d"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","9d2abc900508f4ed574cf5b21fc4eb41"],["/movies/index.html","90bd9625bbbd697011825cfb1cb3f1d9"],["/music/index.html","5514b2ac68bfef662365b0d83103264e"],["/page/2/index.html","fffd057b19fd4e92534df795175b686f"],["/page/3/index.html","0d84f295ddeffa65e852ec166c90b89e"],["/page/4/index.html","edf52482996ecda6e9c40351526f99bf"],["/page/5/index.html","bf6cef34c896e3f08cb4510a2b149e6a"],["/posts/1021360842.html","e4689651ef88761672d641fb8a73a14d"],["/posts/1120620192.html","315f03e63df5b922b714efc956150968"],["/posts/1141628095.html","6f98ee61b27a57897db705d69f5c732a"],["/posts/1168613674.html","67d22dbe80fecf24d3ada0c396ab5953"],["/posts/1219920510.html","9f9d564d05f82cc0bddee8bf7332a964"],["/posts/1222166338.html","a76e66162b5f182cb483883b58fc8fcf"],["/posts/1259097482.html","96bd5ca78f72b6eca61421f45b3de560"],["/posts/1271036369.html","a7147d7f5c543c5e11d791b42edc1531"],["/posts/1312847445.html","7c78e1c02c35bb23e7bf46dc48c69bc5"],["/posts/135355774.html","b14d89f48c84b06854999e1d414a192d"],["/posts/1375344716.html","c836aa2d68cbec36a44d72ed3593d19b"],["/posts/1388991698.html","95bcf18237d88b78de6ed438dbfe11d4"],["/posts/1410315814.html","43a63ca4f908e0606c81264c724fd132"],["/posts/1452790229.html","3cc18d51988ed41b8f83d754bc6e2fac"],["/posts/1470079884.html","7062ba78e5919a662ae63307d8d892ec"],["/posts/1470079885.html","a8f0e6b1731a24523db8a216bd80a957"],["/posts/1470079886.html","85b1abd7d6c1153db5c2e90c33bcde1b"],["/posts/1470079887.html","906e9db95fecb4ea29fa4f9516157de2"],["/posts/1498536549.html","8192b56490a0566fa8c7b63edf2afc8a"],["/posts/1557866301.html","fd62c2707561fb93e58455ce367670ee"],["/posts/1571776361.html","4a9ef3dc4f07549587f79d591c956d43"],["/posts/1605124548.html","fea2a6546e9142f2c937bcfa2e1bf722"],["/posts/1633036852.html","6503deec0798f2ea029e5e893cc7622a"],["/posts/1765123828.html","a34edc8820a4e367e243287eb7b8d874"],["/posts/1776114197.html","d8a7226dc0f9d0a199d6618780dc3a50"],["/posts/1817748743.html","d6629be0c9794531cfca62c6387d31c4"],["/posts/1925125395.html","5f4c9cd98a119fe3b9f0ebb485d0ac1c"],["/posts/1966191251.html","4df84207579dcc70bd68d30582b8586b"],["/posts/1987617322.html","c27a472d3cb1184fa19047d4f1e5474b"],["/posts/1999788039.html","d92db360eab48517bae4de37ea565cdc"],["/posts/2075104059.html","0c3d1ee0a8fad25dfeb4dbf8d6402628"],["/posts/2087796737.html","d58a25d16ba02286accf9da6d660cd91"],["/posts/2106547339.html","364f34203c05015654bb6b2e8e59f5ad"],["/posts/2207806286.html","335ed91032348c1025917f55ab3eee36"],["/posts/2225903441.html","025c21100c308eb94cc91d3617e816f2"],["/posts/2265610284.html","fdac39eec219f317bb429bd088d8775b"],["/posts/2281352001.html","35f23f2904c004b43a9d1a77446dd376"],["/posts/2364755265.html","79d669f0788db2f59b15090e63661c1d"],["/posts/2414116852.html","242ccf5e678e21080bf45683b95b52ba"],["/posts/2482902029.html","73165809c51bf9f515ee110e8e9784a9"],["/posts/2495386210.html","a050a69a9ab091af9ec89f3a49f1284a"],["/posts/2516528882.html","3d40ab5a27c231a3b7560a0c0895adf9"],["/posts/2526659543.html","7b82a387b4d3041292d37f2ff2d58386"],["/posts/2529807823.html","77070df38ec37deccda1301b03d16394"],["/posts/2742438348.html","03ca77a0d2a41d749d5cc9c2e2a7e78b"],["/posts/2888309600.html","e0b8fed33937012e3bd0987d86df858d"],["/posts/2891591958.html","64380217c526ab4033dc6919d8a17c61"],["/posts/2909934084.html","9ed7bf9db4d2d24323b00d133a390cfe"],["/posts/2920256992.html","da3f535e60c4ab584721621a4c7e9a29"],["/posts/3005926051.html","7ddfa1732affbda58d007bd4d8bfc4b3"],["/posts/309775400.html","14e21a283390276bc0e446014e8ad3eb"],["/posts/3169224211.html","81c9c9d9308d6bd5602658f815e4d91a"],["/posts/3259212833.html","0f630b3bfa6bab3566dfe5b0498accb1"],["/posts/3266130344.html","979e7c1260bec489248b7a58d6ac8a16"],["/posts/3306641566.html","42e61009818bc8b547d5d9d52dc83bb9"],["/posts/3312011324.html","26a5804a3ecf027f8f0913fa8072fabf"],["/posts/336911618.html","72926ab5321741b20bb1a0bf4a62d4b8"],["/posts/3402121571.html","8eecd382a7887d8f24a941df6f2b4b6c"],["/posts/3405577485.html","a3dc360b14ce7cb74b646bb3f9a6ed69"],["/posts/3498516849.html","6585965a22a2a6c913cf67c7520e5e90"],["/posts/3513711414.html","06e8715eaa1fc064102969b9ac15659b"],["/posts/3546711884.html","b6dd0e32edf789cb53317aa0f90457e6"],["/posts/3731385230.html","174aa75dba867b1feaf63f5ea4128fde"],["/posts/3772089482.html","f737bb3af04e0ac820cc3695d24ce445"],["/posts/386609427.html","f0fa41afdc2feae837c8be6f3a058c97"],["/posts/4044235327.html","833863e05b19bab70ec88c278fa62c5f"],["/posts/4115971639.html","ad91c682d58b95a960ab3b15ae49cff6"],["/posts/4130790367.html","0f8648919b0761030e568ce8317e9ef3"],["/posts/4131986683.html","0e13cd4a0b6b48a9a27c5df98fc23956"],["/posts/4177218757.html","5480df4073d2159786d677079d098edd"],["/posts/4192183953.html","267d19663e2dae6c56b5f32bf3436d2b"],["/posts/4261103898.html","a23377cb67622def05b0062abb343bf8"],["/posts/469711973.html","b3034e8e0a4e30e4870961917b6e656c"],["/posts/482495853.html","a7f1845d611b0579ac2c2200fbddf2d1"],["/posts/488247922.html","0f58e299be2124db9eda07dbbee83d68"],["/posts/570165348.html","65f9b08003d85c98a474d87558ff7c1a"],["/posts/595890772.html","284ef986d80fe2196ac4326546c99bb9"],["/posts/694347442.html","15d03e988550392b2ca398c2b1ebf189"],["/posts/707384687.html","a5a568523a495801e6653764738d571c"],["/posts/71180092.html","d28a1b37fefd4e73027faf4ccd156bbb"],["/posts/716459272.html","a1120a61d6451c168a79cf1d63974852"],["/posts/795397410.html","90aff022ec0772328e231d0dbfe032d6"],["/posts/820223701.html","473ae00927bfa9a893a5f535b14b37fb"],["/posts/830372185.html","7021dc11e2497961dce354b10903873a"],["/posts/88294277.html","6a343a3819e3b3f97543ef95874801a6"],["/posts/939963535.html","f031f59142d579f88346161d8fe7f1e0"],["/posts/983786067.html","a6f077b7e3b0c10017064727c26e27f7"],["/sw-register.js","a4418f1e761bf298dabfdcf0725c6f6e"],["/tags/C/index.html","1805537a15dc803d27af6bcf4677c23d"],["/tags/C/page/2/index.html","c9bae61d1b459ac9337b182b1fe81f56"],["/tags/C/page/3/index.html","7698e45bb73dccd93f735c88d13cbb59"],["/tags/ElasticSearch/index.html","db6b2e8cb8f27a47df91697e3b8e49ea"],["/tags/GUI/index.html","ac1734da7e9239f8f33075cc4d590e35"],["/tags/HBase/index.html","f2920c49c396b321445a0545704ab616"],["/tags/Hadoop/index.html","e78a1984fd231db1c36991d4d7cd1290"],["/tags/Java/index.html","aa18e23cfde36229f142b33f79e520b7"],["/tags/Java后端/index.html","8990cc6989fd1debebc5e670a0f7b69e"],["/tags/Java基础/index.html","a405942839b87ede65f697c2a64df92c"],["/tags/Java基础/page/2/index.html","d2747724db71758437b7f57b02e9321a"],["/tags/Kibana/index.html","597782082e6502e9cadd4c39dc6e40d5"],["/tags/Linux/index.html","6ae103beb2f7fa68e9e52d52ac62719b"],["/tags/Linux/page/2/index.html","4a8cfe23db584c54f2fb8c880c1368e7"],["/tags/Mac/index.html","f6fe047382bd05166abf32231bd36822"],["/tags/Mac/page/2/index.html","2bf203675761f1c84e449a9bb4a1a2ac"],["/tags/Maven/index.html","86621ac8c3d82097f9ec207906ef7b4e"],["/tags/MySQL/index.html","3d0652b07ec898209a70b9b9edbe0d4a"],["/tags/Python/index.html","44b36a79886d5788bdeb7157d5680932"],["/tags/R语言/index.html","1747385e927a923827ab74a32d16cc8f"],["/tags/Ubuntu/index.html","13dfd852dbe0058d040191c3aa037c55"],["/tags/Windows/index.html","8c83544fcacc21ae303f4f762d3fd849"],["/tags/ZooKeeper/index.html","a9af879b6d2d3634ab7482dc7ee19ba0"],["/tags/bfs/index.html","ce560ef8132a32efcd16d798df0990ed"],["/tags/dfs/index.html","7650021e524a2d32b0fe99ee1dc20080"],["/tags/folium/index.html","9393cd1cbf2443ee84fc6fd33ad14c0f"],["/tags/git/index.html","c573114467668c6fc1f089b4b9f874f0"],["/tags/index.html","c0afdda64db675eb3d40a6256456fd19"],["/tags/latex/index.html","1eeabd3ad667ea647dbac4b3031391fb"],["/tags/二分查找/index.html","3dca1cd19ad1986586d5067763e6fd69"],["/tags/优化类/index.html","78d4312e1c2303dff40c1c76be1703eb"],["/tags/前缀和与差分/index.html","b045deda1bd2d7cd86d859fdcafc0c78"],["/tags/动态规划/index.html","c6683f5da4a7b15d2a4ecacee8af7226"],["/tags/动态规划/page/2/index.html","6613baffb5b165a54b38b357f8876e81"],["/tags/博客搭建/index.html","973ebdafdc55a446a40f4923373e8e36"],["/tags/图论/index.html","8dfbd0d98fa04b6d5d7c3bfaecc09a6d"],["/tags/大数据/index.html","50d3819f73a454feed0091b70185371d"],["/tags/大数据/page/2/index.html","8b3c537a9fbdc430ceb2586f790ab24a"],["/tags/操作系统/index.html","5d19a75272eeb92fd97c57a151ee9827"],["/tags/数学建模/index.html","ee942a83bb99403740577ed854b3c65f"],["/tags/数据库/index.html","19c63d6df92f64b38c9e150a0e162ab1"],["/tags/数据结构和算法/index.html","4d0d724a4b4fa40250e95997e096a059"],["/tags/数据结构和算法/page/2/index.html","e4f810a4234d5f95b8a53dbb56ffd2a4"],["/tags/数据结构和算法/page/3/index.html","fc1b83bdd3a3dea80a3386d531ed74f0"],["/tags/数组和字符串/index.html","b2f11ff4ce3d1b76bfbf3997a8d36ccd"],["/tags/枚举类/index.html","8e2c1f21cae927f6ec1f7ebb964f4487"],["/tags/栈和队列/index.html","bf5453708fcd16dc505df308a64e2d57"],["/tags/树论/index.html","a6cb93b86fca74c91653658595b7b82a"],["/tags/测试/index.html","875a2893c9d2fc2aac2b07c5eda2e7cf"],["/tags/环境/index.html","1ce6282164d48b5b9cd52cb59044ddfd"],["/tags/环境变量/index.html","1f60245075d4e4e6e962cebbb178a680"],["/tags/绘图/index.html","f2a0f84a6d116650f0962bd80ee85706"],["/tags/编程环境/index.html","e52add5b302f9546b8355a4f1396865f"],["/tags/网络编程/index.html","ba1c7b15054e6a9a888cba8439503e68"],["/tags/英语语法/index.html","6d401ff48419867bb0ba05474e7e0427"],["/tags/论文/index.html","7501566240ce69e9a2e601e688593b9c"],["/tags/资源下载/index.html","b78ced9cc08133fcd04fe00e9ff02ada"],["/tags/链表/index.html","74b966a0becd9d3bba03ea76a026df4b"],["/tags/集合/index.html","bc60769e11b46b6884043c036c5954a7"],["/tags/集群/index.html","b1e9da223bd0c525b043f2d3c1b2347c"]];
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
