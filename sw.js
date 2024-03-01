/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","a59ab5c07c639f1f53baf41df178ddff"],["/about/index.html","2646049b5f80d131199bf08f496ab1e6"],["/archives/2023/01/index.html","a2e1b51aebec60a3b645586052c52301"],["/archives/2023/02/index.html","ce6ebc83cedb2f17c3842a5bbb71d9c9"],["/archives/2023/02/page/2/index.html","2741944370635f8554c8d80bc3e5ae24"],["/archives/2023/02/page/3/index.html","f13702c0d84ca66f2b78487410af77b9"],["/archives/2023/03/index.html","807104c557a088576db7eb05a8d0cfe0"],["/archives/2023/05/index.html","b614c665bb67fbbe6983514afd338490"],["/archives/2023/06/index.html","fc0a8ec8147853e3d09000ac5ee75040"],["/archives/2023/09/index.html","ba275ebe0773fa7f75fe550e2d30fe38"],["/archives/2023/11/index.html","76acd774e18cca1e19d022590b18dc54"],["/archives/2023/12/index.html","f5871a842e698726543c8edc72732ffa"],["/archives/2023/index.html","4bcfec025582743168ed928ff220caf8"],["/archives/2023/page/2/index.html","67cab9387379aaeaa95262589b3447f1"],["/archives/2023/page/3/index.html","44309bb501f0d32873919dcc9f81692f"],["/archives/2023/page/4/index.html","472ffbff7e1c584948b1275a4f96045e"],["/archives/2023/page/5/index.html","dbf4795b4f458fba202f05b24a093826"],["/archives/2024/02/index.html","d09b4b77e85e90b098894c9a92fb3e75"],["/archives/2024/index.html","697a615190be6de65de5562cb782d795"],["/archives/index.html","5728664af18d2eef2740f476f1603e86"],["/archives/page/2/index.html","bbbeae86eb4364a47b9c340070c78446"],["/archives/page/3/index.html","663e85838b487bf82def557fbd7397bf"],["/archives/page/4/index.html","3acfd351de3a9282b4e19fe4e04981b6"],["/archives/page/5/index.html","1ed5d117929cf0db7173a0ec05adebcc"],["/baidu_verify_codeva-qQP2iZOMLX.html","fe461519275f8e46337b9984db09d7b1"],["/categories/Java/index.html","897e4004c13aca28f46ff37a5ba3521f"],["/categories/Java/后端/index.html","02cb9863b8b1079e944ead0c3408a984"],["/categories/Java/基础/index.html","73058a4e82941674a65e59ff97c12d29"],["/categories/Java/基础/集合/index.html","8e0ed4b4abce8f64e9de012416c2806f"],["/categories/Python/index.html","8c84ffe7934be030077322f3aab39405"],["/categories/Python/编程环境/index.html","356dde5448776450340948fb345c5a60"],["/categories/R语言/index.html","f371c98a94821f407d584e8606c9c3e2"],["/categories/R语言/编程环境/index.html","c3ab073d03d96b35b641586615e258a9"],["/categories/iPad/index.html","cf52a2b5d79125bf2d82d68299528331"],["/categories/index.html","0ba53c4eda40976014fd649bcf4c61e3"],["/categories/中间件/index.html","3ee43b97c32478b1f1b7dd6c73989276"],["/categories/前端/Vue/index.html","61f1fa48c91a0d1b573c714d55952f4b"],["/categories/前端/index.html","393905ee6a713f3985d293279827da7f"],["/categories/大数据开发/ElasticSearch/index.html","194458e0fcc506baa85afc8e27e19986"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","6c8c80ab462d675108aa5910c0c582d6"],["/categories/大数据开发/HBase/index.html","58ef23f283a558818e52026fccc2880c"],["/categories/大数据开发/HBase/学习笔记/index.html","96c8e95f333f24c0c57baecf41a9f808"],["/categories/大数据开发/HBase/环境搭建/index.html","777c2b81ba00e8b4178e00f343785891"],["/categories/大数据开发/Hadoop/index.html","03ae11110f402728139c3e41556c8fe8"],["/categories/大数据开发/Hadoop/技术/index.html","807cb6af9f51dba6947ea4998043eb19"],["/categories/大数据开发/Hadoop/环境搭建/index.html","b18cbf6dc3629eedca3fb87d0b176293"],["/categories/大数据开发/Redis/index.html","47a5dd0a2e584a8953f1a69f8c2788ce"],["/categories/大数据开发/Redis/技术/index.html","edf2afb301acfd5adc8b5a7c7a6e3ade"],["/categories/大数据开发/Redis/环境搭建/index.html","264a8b4ef69d51d36ec465b70caf13ef"],["/categories/大数据开发/Spark/index.html","3ed8bec6618350f8c4bcaeb37d4ece9e"],["/categories/大数据开发/Spark/环境搭建/index.html","53ead610eabb1fff7776c17dd90229d9"],["/categories/大数据开发/Zookeeper/index.html","38870d39625e70d6db71b3f0f42d1cb7"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","1be5b0ee7edf4d56067cb17c1cad148a"],["/categories/大数据开发/index.html","97fca1a263929ec96133f1329818d3c5"],["/categories/学校课程/index.html","fc8204873518229eee81b2641023e70c"],["/categories/学校课程/计算机操作系统/index.html","43a52eeda91ebd1a081f99c5810a960f"],["/categories/操作系统/Linux/index.html","105cbc7eb402af8b4d18c12d3d67b255"],["/categories/操作系统/Mac/index.html","7ea21831a3e43bde6c525125021b402f"],["/categories/操作系统/Windows/index.html","9fbedcda7d123739625db6fad65b11d6"],["/categories/操作系统/index.html","6216da5854f1b117717e3265a1b1e25f"],["/categories/数学建模/index.html","d94991f739dae332fbd1ff4c298356e5"],["/categories/数学建模/latex/index.html","de4155dff48e84babdc915a3759f5d04"],["/categories/数学建模/优化类/index.html","35cfdd59cac2ffe146dd69795f313d57"],["/categories/数学建模/优化类/现代优化算法/index.html","729c78225a3d6a0a854d3837269c8830"],["/categories/数学建模/优化类/规划类/index.html","64b7a31d7e77b8257a479126f46af98a"],["/categories/数学建模/绘图/index.html","94f9a418700feebab829f3df2bf3fdbe"],["/categories/数据库/MySQL/index.html","6de26ac38a546a1f762dada35dcc1977"],["/categories/数据库/index.html","dc806fd24ed770c4c59bcd05fe35f58b"],["/categories/数据结构和算法/index.html","b034af5c094505302f9be04b91d5dc2d"],["/categories/数据结构和算法/page/2/index.html","8f370bed9f5fe508fd6bf3d818a71d32"],["/categories/数据结构和算法/基本原理/bfs/index.html","16411da5ae4fada6d932ff90f63d5aab"],["/categories/数据结构和算法/基本原理/dfs/index.html","b51d99a8ed4281978a035053992f0db3"],["/categories/数据结构和算法/基本原理/index.html","23f770e49f045c82c95b2abc53caa6bd"],["/categories/数据结构和算法/基本原理/动态规划/index.html","dd34ff341d9b70e53b381e658a7a7aca"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","ac836d89ef0ccbe94b708e47897278ce"],["/categories/数据结构和算法/基本原理/图论/index.html","da4826eeb62076836d424765084f8fd3"],["/categories/数据结构和算法/基本原理/字符串/index.html","da47549f036da969fe74f495fb0b1573"],["/categories/数据结构和算法/基本原理/排序/index.html","28bad7f8becaa60d6530e5ff5116d89c"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","dfcf86da4578e9e1aa5e6d270296963d"],["/categories/数据结构和算法/基本原理/数论/index.html","980840260fc1ac35c7e896b1a95bd112"],["/categories/数据结构和算法/基本原理/树论/index.html","ed0550648bf991071b0340e1c9d436cd"],["/categories/数据结构和算法/基本原理/链表/index.html","68bd3b5888726c9bfbe00accf33e1f8a"],["/categories/数据结构和算法/算法题/index.html","46aa4df36abf276be916b7e95d043ec6"],["/categories/数据结构和算法/算法题/二分查找/index.html","41caf444904c10fdb6c220d66fb918d7"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","4b2d41b534cc966092e31ef413015b92"],["/categories/数据结构和算法/算法题/动态规划/index.html","964bf0886d84321bc6a0c2033223ef4c"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","8f0c4f4855363975fabb5e459296b4b7"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","4f9434879cc0ea01720e68f0eca815c0"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","9c6317aad203e62b455cf0e66c3ebec0"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","01222dcacdbc2c4e8ffbc0669cf456d2"],["/categories/数据结构和算法/算法题/数论/index.html","6366ee83fef96395552c111b37792f1e"],["/categories/数据结构和算法/算法题/栈和队列/index.html","8e6c225a5fed8c0aeea62078cc810eb3"],["/categories/数据结构和算法/算法题/树论/index.html","81edbe417e0f84f3e8db0151de0580b3"],["/categories/杂七杂八/index.html","1e0dd1d25d6df607e6680b3da1e28516"],["/categories/杂七杂八/博客搭建/index.html","c8efbee5ded6e4dbd3aedbdef1ba5943"],["/categories/编程工具下载/index.html","661372968fab39d72b2212c36f971b4a"],["/categories/编程环境/index.html","0894d73059eeedaf9bdf38deda224666"],["/categories/编程环境/大数据/index.html","5d34db1cb861a56ef4ab66d1083f56c2"],["/categories/英语学习/index.html","b40252dc78f8a88ce905fe7582769a2e"],["/categories/英语学习/英语语法/index.html","b976b872cb3a93a8fe04d07abc052055"],["/comments/index.html","4499c08f6769172cafc73d8baec34f05"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","3029e25ccad9981f1cdfe17667dcec92"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/apple-touch-icon.png","f3c6c1198d76dc58019752df6df79f1d"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","e10c3ca564b526be9818d7e237572723"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","34efebb89a7fdfabb816ff5e9509847e"],["/movies/index.html","166bdec777726d73d3b8ffa5cb2aedf7"],["/music/index.html","8d4fb924d15d528060e3ecf450d1fc78"],["/page/2/index.html","f5da4e5091998ee40a40ecf59b2d9eb6"],["/page/3/index.html","cd3d2866a2b984f78fcaf0e548f7c00a"],["/page/4/index.html","97e47cae78e39da5e8fbb40afe57d1f6"],["/page/5/index.html","764929de17748f5254c946b0cc83f808"],["/page/6/index.html","aa6c31045c10d557caf1ea3c85b96e41"],["/page/7/index.html","884146f65e1648935cff9e6a3290b62b"],["/posts/1021360842.html","cd86098135a0bb53f0ff2dc98f726db1"],["/posts/1120620192.html","f3b63a04f2c080056dcf78800c1e864f"],["/posts/1137707673.html","3e32037ffa1ef92bc5288b4c83b60ddd"],["/posts/1141628095.html","c84b8f6546a82e36690d2f9f84efb408"],["/posts/1168613674.html","de7ddf5f7a1c68e77e1f5576277b4501"],["/posts/1219920510.html","fcf044d63fbb02573a17cd419fa3afb4"],["/posts/1222166338.html","4edbc415f9543742710aabfeeb1a3517"],["/posts/1259097482.html","8bb8d2cf338165b33d24c002cd876942"],["/posts/1271036369.html","03883c0786bd3a5a9d4221fc98645ee2"],["/posts/1312847445.html","2d25ffe1ac3fc22091d605ec5ed849f0"],["/posts/135355774.html","542bd012ec19345441c04910208fceaa"],["/posts/1375344716.html","efec7028a545227f565d512e3e31a6df"],["/posts/1388991698.html","19195ee607a81514cdd2eb4a953309cb"],["/posts/1410315814.html","c5fb592fbd9cb4e253633165b28af02a"],["/posts/1452790229.html","ed070333131163d694742a0a2e43d8f4"],["/posts/1470079884.html","15b3ac47e9dd0a048fdd7501460889a3"],["/posts/1470079885.html","ef438825c0df5e963f55193a22787b1a"],["/posts/1470079886.html","ebbf2d6891be0b828f63af11db490ccf"],["/posts/1470079887.html","d95c3ef7d0f0949330559958635a41ea"],["/posts/1498536549.html","2f7e4386df0b7afed52c7b4a2d9bdb98"],["/posts/1539568593.html","c75b3012b432d32f8ee82983379e5ebc"],["/posts/1547067935.html","5cc01f58678cf3b9adf15cf912b17469"],["/posts/1557866301.html","fd907046a0aa811f6019047052eed530"],["/posts/1571776361.html","ad1604071ca3e62062fee226f9d58bb0"],["/posts/1605124548.html","31b8ac6a97fe57085f12b9658fcc6c2b"],["/posts/1633036852.html","ca4686c5d40b306b3a13bd89ac861cc4"],["/posts/1667740714.html","dac41eff02f36c4cf9c0d7a8756c7f27"],["/posts/1674202625.html","5e77cd96522b2b28c97bda6ec1634e80"],["/posts/1765123828.html","ae102fa41edffd4577fa42203e726aa9"],["/posts/1767336200.html","6e6d571bae062108d276fbcb4ebca274"],["/posts/1776114197.html","9277b6f1bb4ab727a965fc404a8f97e6"],["/posts/1817748743.html","8a6fa73bb6f78655c46076965e347ee9"],["/posts/1925125395.html","ff6fb56484bd85fab917e5deefb2b990"],["/posts/1966191251.html","dcb0a53beac7d0e32524daf5644d44ac"],["/posts/1987617322.html","efd31bc70aff5f167db1e2717e999004"],["/posts/1999788039.html","df9d0781645b835c1451c2b968757c07"],["/posts/2075104059.html","11960afed1bc2e3ea8c15790b5d535d1"],["/posts/2087796737.html","dadd3c29f4319163422a3f42da4eda5f"],["/posts/2106547339.html","e3d20e36cde1ef45eaa3bd5f1e3f3bc8"],["/posts/2207806286.html","90dd47c6502ea9e9f9888bcbf07b8bb9"],["/posts/2225903441.html","13f241b8615342bf418e77e419b3d359"],["/posts/2265610284.html","17a16031eb2a65c77934fa50d8dda69e"],["/posts/2281352001.html","db5ed640770d73cc51ef35b09370cc48"],["/posts/2364755265.html","f751ae849bdd49b1c703dff2fcc1897a"],["/posts/2414116852.html","26138977e0c799259a6d7d1439749533"],["/posts/2421785022.html","0deb01bdb5104f52cfbca7b0a2632f3a"],["/posts/2482902029.html","03da49dca1f92d705b139b26ca3da490"],["/posts/2495386210.html","68e583a05550ed7b4ca6c667e94857c8"],["/posts/2516528882.html","17247bf4398f71d144f8ba500533f5de"],["/posts/2522177458.html","d37637ff2ffd6b217ab4212ea07bcc7f"],["/posts/2526659543.html","69df277e2092e2dfca7104f49eb5c8bc"],["/posts/2529807823.html","3424d41c999b51898479d413eb6f9797"],["/posts/2596601004.html","51491b5b4c5d85f8c469992020a907eb"],["/posts/2697614349.html","0ad53ff0a91a2cbc345ed7e61f88b6db"],["/posts/2742438348.html","048d5e092fa91d36d46e20da6f1e33c0"],["/posts/2768249503.html","27d0566934cfa72915baebf6d03a748a"],["/posts/2864584994.html","2dc49228981e20deb30937addf975cee"],["/posts/2888309600.html","d317331093835400906b0a53f887055e"],["/posts/2891591958.html","a1bdf78f2f64ae1915043812359991da"],["/posts/2909934084.html","a1dbd197326597a71c9125c4ec14ad3a"],["/posts/2920256992.html","87eef1083ad4e35ebb4d35e135d6bff0"],["/posts/2959474469.html","35567faf1a96db1e44d8f8fd8f1b2944"],["/posts/3005926051.html","02721da64690e62c6b900452eb5a7ac2"],["/posts/309775400.html","a7c22730123106ae99b0b6afd2961341"],["/posts/3156194925.html","124b25a718003cee798c19613fa4a22f"],["/posts/3169224211.html","e144c7025582f9230cc7a6eea013f4b7"],["/posts/3213899550.html","7c1899e75a018326a265d4e168857a4d"],["/posts/3259212833.html","166c248efaa32af315753e88ff74bd68"],["/posts/3265658309.html","4ed35eb973a9761a4201a4b00270eb4c"],["/posts/3266130344.html","b18766d5baec105a29a9635e71fa3e75"],["/posts/3292663995.html","bf2a1b4cf425b30236b3728ea5ec94f0"],["/posts/3297135020.html","c63ad9569d0430d45339612e2489c5a7"],["/posts/3306641566.html","e9a84286139784206edb1d587fef427d"],["/posts/3312011324.html","be44255afbb0dea98546b9c252455b61"],["/posts/336911618.html","d920af7b998150d65b667fa6a0a69776"],["/posts/3402121571.html","b53d8f036eca30a9c96fd91274326e80"],["/posts/3405577485.html","57786d75fad9f2d88508a094eaccc87a"],["/posts/3498516849.html","f8a48dea641b05c968c85817520d7431"],["/posts/350679531.html","809cba4167844e3a1f3526696b780df8"],["/posts/3513711414.html","66574861f1131cb8480be7f22cbc62fa"],["/posts/3523095624.html","47a74a7a780416b361cba4ccfed798ca"],["/posts/3546711884.html","0f41ccb9a75cf641ab3f032d36e794f1"],["/posts/362397694.html","22cee66c713b77e4f0fe3b604440c4d9"],["/posts/3731385230.html","ad4e0028fbc097eb1d46cb4c613c4a1d"],["/posts/3772089482.html","81b37ea81421b54b469a69d33edda97f"],["/posts/386609427.html","963c869346ce70fdd19cce5bf3b9bb4c"],["/posts/4044235327.html","25b986adbd97a192d6fef98e39460d61"],["/posts/4115971639.html","4a7a3b7aab3b801ece8e6fb3a01ceb9a"],["/posts/4130790367.html","c8600c7979c7ce27058e2fb3e8b4fecb"],["/posts/4131986683.html","6c397840ecc91dc337226e9dac72dacb"],["/posts/4177218757.html","688deeb0a5ffa9d4a88a3e018969aee6"],["/posts/4192183953.html","b14015f25074ad368f8142888b0b01b8"],["/posts/4223662913.html","01d2b91e242a839b24ca3c4c9eafe631"],["/posts/4261103898.html","5296ae7d8d9f1773b1f401a64b234aaa"],["/posts/4286605504.html","4813136e6e1d80e59fe9e4e5db4ad8af"],["/posts/449089913.html","a37c238ea01904883c9c21d9c87b6aea"],["/posts/469711973.html","8e64326bf16d655ce41379b3f4bc9921"],["/posts/482495853.html","4fb97bf20bc08ac6f0ffbecf8c364fc5"],["/posts/488247922.html","ebf5e8f654a82be4bf043d31e374c443"],["/posts/517302816.html","678a8d413439ca0f62ffc9cb96775b08"],["/posts/570165348.html","a6c4b142dca530513608f2439dae7bec"],["/posts/595890772.html","229d450f3aece6b6d7d3052709c78bad"],["/posts/67485572.html","4f88a87663b1704d66d59827bfbeffc4"],["/posts/694347442.html","0ab7c5bd4862dd162eb4f90f854e70e8"],["/posts/707384687.html","eff86527339ddf0a99259c7b6524d0a2"],["/posts/71180092.html","241e7a017178121f6f2568a150d6ccfe"],["/posts/716459272.html","e605ef0d7e4d229a479daed09e5abfeb"],["/posts/765481613.html","e37480bc3ef73154dc310f9843ee1b67"],["/posts/778231993.html","32027371cb3720002c50b2b7f32fc52b"],["/posts/795397410.html","af4508043e2288e66e6c6266f74ca8e0"],["/posts/820223701.html","b665dd4002dc9ef9b5f1c4c883de0357"],["/posts/830372185.html","985380c09fc2da56de85b994b07247d2"],["/posts/88294277.html","51c9041536e89185ccc43fe34e761bba"],["/posts/939963535.html","e22362ac01c71b5b79462d23d3ed2000"],["/posts/983786067.html","6b6f56668744252cecfdb6bb6a45a943"],["/sw-register.js","48f709ebba8b4fa93c5ff17b6312fea1"],["/tags/C/index.html","31bc912d1371d9b6060bc2c1e9425a51"],["/tags/C/page/2/index.html","b95d5813d82a183810c93207c1b6afc7"],["/tags/C/page/3/index.html","e40fd3713b172eebf379b258617aad8e"],["/tags/C/page/4/index.html","76253618fbf77d8ddfcca0375ad4bed5"],["/tags/ETL/index.html","148829dbec1310b5a371415341da694f"],["/tags/ElasticSearch/index.html","05d6cd9894e7c5dd2360393041a7eeb3"],["/tags/GUI/index.html","396d11a827462c166c3cd91c383e2285"],["/tags/HBase/index.html","87cfea0791e90f370913b68177616483"],["/tags/Hadoop/index.html","7b7d2e6e0c6648b53544bfea00e04f25"],["/tags/Hadoop/page/2/index.html","cf590dfd03831995c7f8d7f459b32261"],["/tags/Java/index.html","aba4a0e828bbf9a7ec0e4e23634db942"],["/tags/Java后端/index.html","9dfd2adf33651d83a5ceb892c18a32c6"],["/tags/Java后端/page/2/index.html","2602ae7bd20a315caa33fbb0fd3aebf0"],["/tags/Java基础/index.html","14fe2a10e7ec0c84542cd166386aacd9"],["/tags/Java基础/page/2/index.html","3a869ec3b6b16810055d86aeda23aefc"],["/tags/Kettle/index.html","810b397d999884616deb6334f34fb13a"],["/tags/Kibana/index.html","65fb7897d8b4d3a747d9dfa14ec65c34"],["/tags/Linux/index.html","921c9e4b53f3521bc6bdd4a5d2e69bf6"],["/tags/Linux/page/2/index.html","ea5593dad10bd7cdf622335aca6d0c65"],["/tags/Linux/page/3/index.html","b058d0c1a48eb2fdb1bbf3dc86009383"],["/tags/Mac/index.html","5d98b1ca98049f7831b2cc2ae549af59"],["/tags/Mac/page/2/index.html","af8957d03d18dd34fa795c5f0e44c76a"],["/tags/Maven/index.html","b9f37aa57c0d92d0e8fbcc4b6cb4a60e"],["/tags/MySQL/index.html","5e0f7de83104ee90b6ea5207030c77e7"],["/tags/Python/index.html","70252b3638b25ddf1f71ab858b923b64"],["/tags/Redis/index.html","d5f93e9b8f6d1b2a35f6f7ce5a2a9927"],["/tags/R语言/index.html","1c37430473f97b0f491e533d7380fc48"],["/tags/Spark/index.html","5ec6520e2ff232e2ee602a1dbd5cf97a"],["/tags/Ubuntu/index.html","e7103b3269b6b9c38875c0ee82c3e5e5"],["/tags/Vue/index.html","b088144187ab78879890d408c510a6c3"],["/tags/Windows/index.html","be10be77df6e84c1a832a1007fbc77c2"],["/tags/ZooKeeper/index.html","477bfb2eef75e6cda55bf64989a5d7e3"],["/tags/bfs/index.html","17c5e3eb25c127d5a7cf05a29e88e6c9"],["/tags/dfs/index.html","f07308c357ea3200fbc22bd293fdc3af"],["/tags/folium/index.html","d31dcf2d092c7fdf207658ec7f2eb4ec"],["/tags/git/index.html","07d1140dc3376833ac87f2ad8899b9d9"],["/tags/iPad找电子书/index.html","99a3b59c9b181ac73d56ad2cd9a6c439"],["/tags/index.html","f99e792d9078e0d4612bfd69d3d54ab7"],["/tags/latex/index.html","a1d382333d3fd68780a437c89629f31d"],["/tags/中间件/index.html","c228e353cf610dccd237dd55de6c8de4"],["/tags/二分查找/index.html","3a3ca6d1ae75d4d84e918b48e3be239e"],["/tags/优化类/index.html","ecc3b00b508fd6cdfa368d21181ae3f2"],["/tags/前端/index.html","ecdfd887516be5b8758d5c5dfd1f9fe5"],["/tags/前缀和与差分/index.html","dd7c801a7038731a767c7b0477f65b6a"],["/tags/动态规划/index.html","cc320f2dd0cfd5a83e9f83e94d10ee10"],["/tags/动态规划/page/2/index.html","053d4a74bf4837c635173c9bd1d86e7c"],["/tags/博客搭建/index.html","9994a0d90eb0e5d0d92263e7af098a92"],["/tags/图论/index.html","75fd536aaada30d12392f5bd33e255e1"],["/tags/大数据/index.html","ae2973eab87a04c85077e3e74513286f"],["/tags/大数据/page/2/index.html","690f8bc7e35a04b41ec4e1b9c6c85e8a"],["/tags/排序/index.html","82c11bc7e4641a591a92c5dd7dc81080"],["/tags/操作系统/index.html","c45f9ba8e6a949d29bd01cad72a6df89"],["/tags/数学建模/index.html","82c4ca242f3524df27148d0c4d8e25ca"],["/tags/数据库/index.html","62336297a51010509be23f12dd309c99"],["/tags/数据结构和算法/index.html","fe8a38137fc0a2c204a124e340eaaee5"],["/tags/数据结构和算法/page/2/index.html","87de2e6aad91e7004d212f26d166ff95"],["/tags/数据结构和算法/page/3/index.html","075e391e0221550a19cfbc2b74b1b2e7"],["/tags/数据结构和算法/page/4/index.html","5bdded2ee71c5c077c0bc6063580123b"],["/tags/数据结构和算法/page/5/index.html","2f73a83a1bdcc924cc6887681de42e14"],["/tags/数组和字符串/index.html","ee7e71a5931029d7b9929dd62ea59c3b"],["/tags/数论/index.html","2757ff4df5b8bf51e988133cf0a01d5f"],["/tags/枚举类/index.html","0a7ad736a4a48e5e0eecee29c5e5bdd7"],["/tags/栈和队列/index.html","bf3b7c288faa2011e44bd981fba91abc"],["/tags/树论/index.html","10ebbea652cbcf04655de4f67dff6f50"],["/tags/测试/index.html","a577a1a8296fdd3b219943e0bbc1d5f9"],["/tags/环境/index.html","9791eceede60df7be0adc798c1db32f8"],["/tags/环境变量/index.html","4d833512569e76711f11796d4488f50e"],["/tags/绘图/index.html","78cb2863544fb6dd68799efb48b65453"],["/tags/编程工具/index.html","d16e33d105f0a87656f45e7174685484"],["/tags/编程环境/index.html","620f71ce4ae2998d4d91850e444b7d74"],["/tags/网络编程/index.html","33f0247d020109bc80c4775560feae39"],["/tags/英语语法/index.html","3d55db5fccd99f182a5caf1b816ae250"],["/tags/计算机操作系统/index.html","4e1bd9cb721de866255499a7cc68f72c"],["/tags/论文/index.html","d07682f4edc619a4d30d757dbb5708cf"],["/tags/资源下载/index.html","f8fb0724b30cabda3b8a718b12ff55ec"],["/tags/链表/index.html","b09d602a9a7d677f263c0db29996dd86"],["/tags/集合/index.html","0b115ca8881f9192b114bcdac0e3c4f4"],["/tags/集群/index.html","6b8b0eae18dcbd945ea7667e8e8f3178"]];
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
