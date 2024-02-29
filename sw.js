/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","643c40c2122db95ba92172428a646f72"],["/about/index.html","f3847a73f649610826ff128d196a0496"],["/archives/2023/01/index.html","45eb85425227c25bf5142d019f0e6d80"],["/archives/2023/02/index.html","d4960e7b6f80b654fd8921d304458745"],["/archives/2023/02/page/2/index.html","8134c11aac5ea9ff9c3bbac0e9b1a009"],["/archives/2023/02/page/3/index.html","03d4e91371794675ed70a35db6b0805e"],["/archives/2023/03/index.html","eb32f5cde636d7d178993e9016ae9252"],["/archives/2023/05/index.html","5a71fd36e75d77ae864bad4c45a10bf2"],["/archives/2023/06/index.html","9817d5b71f4a5cbc88c82723a3950646"],["/archives/2023/09/index.html","28c1084ed0625bcbc0b13d09a4b314d3"],["/archives/2023/11/index.html","87f77e59ac80caa46fbaf1768499aadb"],["/archives/2023/12/index.html","7773f0ba660021d077b73d21b269d33d"],["/archives/2023/index.html","f328fa107975247ba54bd9ca2e1d93b3"],["/archives/2023/page/2/index.html","000743c5ff685a8f86e16a653577372b"],["/archives/2023/page/3/index.html","7f4804fad631ea0da845ee80820b001c"],["/archives/2023/page/4/index.html","3a0362678aa2d6ab7df3100d62a03c71"],["/archives/2023/page/5/index.html","4acaf0196c1daa4a1844a9590a70916b"],["/archives/2024/02/index.html","52c2d0c87b46358e782ef5af04d2fafe"],["/archives/2024/index.html","c857cb03f293a98c8a7c199ac950ca85"],["/archives/index.html","c42038cb4e81fa634069407df0d8c116"],["/archives/page/2/index.html","b061d89cff338f3c01e0923db8262e68"],["/archives/page/3/index.html","b74896e6c07a2b0c504532cf95797aa5"],["/archives/page/4/index.html","c6985ff3a7c1f2faeaec43f86e0fa60d"],["/archives/page/5/index.html","d14cf51a75d81d37e335b19c0799dd8e"],["/baidu_verify_codeva-qQP2iZOMLX.html","436eadb983f360750d816ed636d0f83e"],["/categories/Java/index.html","e68d68d06cadb47b0e315d0fab4d3092"],["/categories/Java/后端/index.html","5fc46ba4b5c85d66e20b75b2def635dd"],["/categories/Java/基础/index.html","3be15f3b4ed098c47361cb89a477b0a1"],["/categories/Java/基础/集合/index.html","4368759515dfad72f79d9c6ae1bbd8e5"],["/categories/Python/index.html","ceb5a0753faa74ca631a10b657971246"],["/categories/Python/编程环境/index.html","e6912f09f031445c14b1d1d77c013dde"],["/categories/R语言/index.html","60fa4c1750a10c3039196fb6b0c41577"],["/categories/R语言/编程环境/index.html","1a895d7bdee61fd249a73a0ba0ab872c"],["/categories/iPad/index.html","7d27d6ff6f036b5a28484ada84b0f702"],["/categories/index.html","647233696305f4bb1004ad830ac0f12c"],["/categories/中间件/index.html","18cee110ded5d1b9e988a0212636dfbb"],["/categories/前端/Vue/index.html","c0928bda1dad0fa7102a45b9e1a511d5"],["/categories/前端/index.html","95d703ae71034d9060f06c99a7beaf1a"],["/categories/大数据开发/ElasticSearch/index.html","73880c3ed9fc7b5430e61b18a6fc84b0"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","426675061fd0b31255037fc0501d946d"],["/categories/大数据开发/HBase/index.html","cfe52aeb2c2885f3da560309375f4a8c"],["/categories/大数据开发/HBase/学习笔记/index.html","6656023ba09079c734b250776e6f2f20"],["/categories/大数据开发/HBase/环境搭建/index.html","aca936329b776d6e0a6d0118f5e59997"],["/categories/大数据开发/Hadoop/index.html","53c73fbfa6f45d8227da390743807ec0"],["/categories/大数据开发/Hadoop/技术/index.html","8ad97fc6221c185a476f3eb4445f4e3e"],["/categories/大数据开发/Hadoop/环境搭建/index.html","ec5b6f14ef0d1170d035863c39b6bcec"],["/categories/大数据开发/Redis/index.html","1093cf19e1c8b3bd5786ab06679990b4"],["/categories/大数据开发/Redis/技术/index.html","d4c8c8361c72eea752bca5ba2c4383d7"],["/categories/大数据开发/Redis/环境搭建/index.html","9c9db4f4f84760c0241be166d135f8c5"],["/categories/大数据开发/Spark/index.html","72c503b042e2c0667fc1cb75fe62c1e4"],["/categories/大数据开发/Spark/环境搭建/index.html","e431d9167d8df040c3d37c01fcfb6ab1"],["/categories/大数据开发/Zookeeper/index.html","225f9af4b7561757fe3b37e96d538557"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","74792b739adb805f73b5dec2046eb308"],["/categories/大数据开发/index.html","1dbe4eeeedf66f3705bb7ce52bc3f415"],["/categories/学校课程/index.html","7e43fc4a95ca9a262b9b4549685882ed"],["/categories/学校课程/计算机操作系统/index.html","5faf95f7f8c6ae466289eb4e2b151aec"],["/categories/操作系统/Linux/index.html","06c2eb279fb1afc57625e2cfddee1449"],["/categories/操作系统/Mac/index.html","30157072ea048815e9d8cc52a963eebb"],["/categories/操作系统/Windows/index.html","598f8110e89c4462d8c6d43ec208cb7d"],["/categories/操作系统/index.html","e630800e8869d3c39b05aa099cbd929b"],["/categories/数学建模/index.html","37d4cf9a7fc1c369295c945f8c7bc663"],["/categories/数学建模/latex/index.html","c5cc21acee8f582b9eab08bdbc1ac0ef"],["/categories/数学建模/优化类/index.html","615a19b45f84c0656d0b408349d18694"],["/categories/数学建模/优化类/现代优化算法/index.html","cb16435321cce5f5337f398a0044e51d"],["/categories/数学建模/优化类/规划类/index.html","7c7e0b4930597615331c86d671f8b8f9"],["/categories/数学建模/绘图/index.html","cb9c01e22bbdc4699574e8b5251a07b2"],["/categories/数据库/MySQL/index.html","299a6418ba0d64c736df45e99b1501fa"],["/categories/数据库/index.html","59971834655be165b0e77425a41a8392"],["/categories/数据结构和算法/index.html","019b84b02deee813475ea339254429e5"],["/categories/数据结构和算法/page/2/index.html","76441b7a43efaad6400053bc0e3e1bef"],["/categories/数据结构和算法/基本原理/bfs/index.html","fbb134e07c0d13682f9023b3c15c614f"],["/categories/数据结构和算法/基本原理/dfs/index.html","662d078268a0f8e2b36c9fae3559d62e"],["/categories/数据结构和算法/基本原理/index.html","94b4b6dd210a6828c19c4228e04ddbf5"],["/categories/数据结构和算法/基本原理/动态规划/index.html","f6846b6b115da86a9bebc84369efc90c"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","cc0283213ef30fc2726775b2c9dfef5a"],["/categories/数据结构和算法/基本原理/图论/index.html","02d07a54b026d25d27a1f928b695ff2c"],["/categories/数据结构和算法/基本原理/字符串/index.html","0f6b7d4ebff3aa7ab4a6f913a3c459d0"],["/categories/数据结构和算法/基本原理/排序/index.html","d1efbe627c930921ac75f46b3a03d0bd"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","42e6906cb64df28e0e6c464780fefce2"],["/categories/数据结构和算法/基本原理/数论/index.html","e6fde1242c5162b552e7275a931bf96c"],["/categories/数据结构和算法/基本原理/树论/index.html","9288a575de8d8b8c0fd9ae9517ebb2aa"],["/categories/数据结构和算法/基本原理/链表/index.html","6c6980ce41255b5692506e51a993bd78"],["/categories/数据结构和算法/算法题/index.html","6255c234d579464242a2a51e4e06ddc3"],["/categories/数据结构和算法/算法题/二分查找/index.html","3c1959f4046f1033eec3a5ae2c085b74"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","cf66b57c9f39920423ef8313f9a21ab5"],["/categories/数据结构和算法/算法题/动态规划/index.html","ccd8905314a6a946f45f4d3b2111de1f"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","86d53b6c2a1ad3c6ee95769be86f2a8b"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","a804ed6be1f21dfbdb05b810a77d07e7"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","e50e815e46a77f261e405821817ac402"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","818648c414390ea9683d9703f4f0962d"],["/categories/数据结构和算法/算法题/数论/index.html","73e3f3d73b0cca2e90a0d9289c577fe2"],["/categories/数据结构和算法/算法题/栈和队列/index.html","dff2223cec192f9e518520dcd788f3c6"],["/categories/数据结构和算法/算法题/树论/index.html","a7f54e15eea62c8ed7f01c6abd82da5f"],["/categories/杂七杂八/index.html","ad72da59411f8b4b1bbb662224313538"],["/categories/杂七杂八/博客搭建/index.html","c7e5dd216bc4ab80201e60c5c44d17e8"],["/categories/编程工具下载/index.html","f9ef5e29bfc38a8f07ae4b45100de5d3"],["/categories/编程环境/index.html","1a66f63e0724880046317d3d056f1db6"],["/categories/编程环境/大数据/index.html","6de5607583c4bf5cf02552d0b1583f0d"],["/categories/英语学习/index.html","ee03c0c08645265e4554b4c10bd8e597"],["/categories/英语学习/英语语法/index.html","984fa1f7533e59871f896f234aeda738"],["/comments/index.html","e4a5d6a55bd96354dcb092638b12bdf3"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","cadbae6edc90b2f277452ddd24b8a931"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","88d32e95fee375d8d0e3f2c3575a4d98"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","3022acc7e1b12b5b912078abc82ad281"],["/movies/index.html","c78d5a86acd9e6ef1fd930a43a082626"],["/music/index.html","92453ae9ac78033140b76cfba0c6b751"],["/page/2/index.html","2aae80561b4196e85390136df02c41f5"],["/page/3/index.html","d1fdf59603cfe2b9c8323f2e40f86d8c"],["/page/4/index.html","14dbc91e5ee36fa9eeafdd00f845da8f"],["/page/5/index.html","0568c41c87833c6c68e88be76f441138"],["/page/6/index.html","fed34a87d4154e34c54f2b72db9f2fe5"],["/page/7/index.html","38fd008720b91fe755b846f10b63efb6"],["/posts/1021360842.html","734dab4716af5c9e6f23b337e4dd5eba"],["/posts/1120620192.html","e2007d505b9121feb5852e43cafc5cff"],["/posts/1137707673.html","2c3846f28de8d943a9c1434e37e82d4b"],["/posts/1141628095.html","4fab5ee70f5c1571ec3e2786c16f878a"],["/posts/1168613674.html","95bd950747a541dc1e3c39a432fb9f6f"],["/posts/1219920510.html","8f8a16151914fa474def2458d0ad8f9f"],["/posts/1222166338.html","795881e7b83d350da6f10bfacb6660ff"],["/posts/1259097482.html","ce2c3d456de7d5afbe170c4bd2ba416d"],["/posts/1271036369.html","e1d814b2e1ac419622e249d33cfa1e4d"],["/posts/1312847445.html","c5cca638aaa39c48f163a5b29ff9ce9d"],["/posts/135355774.html","4a1b930c62c32cc54d6b5c776e941b1c"],["/posts/1375344716.html","02e32bebc46051716d9d0a42a2e675ba"],["/posts/1388991698.html","a3e85c62e26328788ba1cc1fec2d61c3"],["/posts/1410315814.html","4c5da53856faf2593c9b972e8cea736f"],["/posts/1452790229.html","391f85d69a2cf551092c20e5563eb2db"],["/posts/1470079884.html","d31f6faa218af21b64601d62e6c072ae"],["/posts/1470079885.html","580c5784541acab1dd63b390519f2525"],["/posts/1470079886.html","1dbcda4d063d3614494a35a833a50a8a"],["/posts/1470079887.html","8d434e5f1b81f8157caf6c8510eb9051"],["/posts/1498536549.html","ee08cbd5ac7ccbb89e5314d51e39b49c"],["/posts/1539568593.html","d1a85cfd0826342695c716ef9a5e9ff6"],["/posts/1547067935.html","adf22b34e08ec203d529137797112fe7"],["/posts/1557866301.html","95b465c10bd3cbcbea5e6dc41ecbf0b0"],["/posts/1571776361.html","239e033290758ba96ea4e41aa5ec2973"],["/posts/1605124548.html","546bd1a9486305a6e9aadf44dbbc2c88"],["/posts/1633036852.html","40304a014e35bf6ef897b9de1d5d6605"],["/posts/1667740714.html","e7a788f8b0b280dbf24e0ffca654b54e"],["/posts/1674202625.html","0d7ffb9e7c2d716baf855ed985bb7672"],["/posts/1765123828.html","09dd03e617ca7820d31ce2f69d55aa65"],["/posts/1767336200.html","d4ae5d40c9ea5022093baf4316692e95"],["/posts/1776114197.html","511f0d3cc369c3129456f7f5507f0260"],["/posts/1817748743.html","6a09160b4dfa3ffed5fca4c32b645c86"],["/posts/1925125395.html","9e9260b6bf075a0d69b9c402d28b32dd"],["/posts/1966191251.html","b6cadaaa6da54c63ad90f363d755f074"],["/posts/1987617322.html","145ce8d243fa0fd2fe7b626590899c1e"],["/posts/1999788039.html","e92ce857c43221e3d62b519c1f5e171a"],["/posts/2075104059.html","34159c8740201048f9ccb4d763149102"],["/posts/2087796737.html","39f47ee850c7108c459a994b2e6e314f"],["/posts/2106547339.html","9130ed49a1055ebaf1cf5b6995a14d1a"],["/posts/2207806286.html","475f9c357014c308382666042c1ae18d"],["/posts/2225903441.html","020bf121ed2694f491d5bfdce07bcd3a"],["/posts/2265610284.html","f08dbb1331a24091a423d03f8069aa6f"],["/posts/2281352001.html","addee2209706c232b5eeb0d9399d0e94"],["/posts/2364755265.html","b83b54e0ee5d634f89365d3589d8b69a"],["/posts/2414116852.html","90a2b68731ab15f52ec221fd9d1e4db0"],["/posts/2421785022.html","143b4a4eb9c075d1bd3a3ba45eea76b5"],["/posts/2482902029.html","370152986ac9d06861a37bcb6ba8aa0d"],["/posts/2495386210.html","4a141f895390ab1f8918f5447781d78e"],["/posts/2516528882.html","e502d2f5198308d9112e4515f08cc9ae"],["/posts/2522177458.html","072828e9ae339ecdf64ea3bfae8e4a91"],["/posts/2526659543.html","f7d18695991c0b029ce81e9e52b2321f"],["/posts/2529807823.html","4d661e01d26957dbe029e0e478c538ff"],["/posts/2596601004.html","d9d918afb42c1c2354a3e055462386a6"],["/posts/2697614349.html","d3e729012bebccce882d3097e3e6b350"],["/posts/2742438348.html","bd4c837bc0945aa56115d22c50009c7f"],["/posts/2768249503.html","3c052fc2d1128855379a5f1aa968123a"],["/posts/2864584994.html","a6cb452c7e40815241a3cdda879780fe"],["/posts/2888309600.html","2e06934cf71e858fbfa0bd47b0a5f0d0"],["/posts/2891591958.html","9f82ea4b7503c38dcd50daa7808d8c88"],["/posts/2909934084.html","b74f01780427b9c49b9ead01a7455907"],["/posts/2920256992.html","5f8b0b0abee2571ed7532e7af791a713"],["/posts/2959474469.html","561d2b53f5509a83704f54c2d80f4cd3"],["/posts/3005926051.html","eb7cf0c1aad7a7e3b44b645a088ecc4b"],["/posts/309775400.html","4f43b51d6ccf656b82a708fa1903f891"],["/posts/3156194925.html","bb869a0c868285e053814d4d8f7d0617"],["/posts/3169224211.html","836b2eeec57008a0541a700d2ad92b1e"],["/posts/3213899550.html","d83aadd64d0911a7397a8e8e9483ab4e"],["/posts/3259212833.html","b09632c6374bc41594aaae6490c567d4"],["/posts/3265658309.html","46b442a6638d70ed75b99a78810ac538"],["/posts/3266130344.html","39fe33c3a8b719a187b78129158ff703"],["/posts/3292663995.html","fcea07deea19842ea609237235cfd784"],["/posts/3297135020.html","a8c40301ba21140f8f4804841fe11943"],["/posts/3306641566.html","4fc2e5c42d7a83b6a77776afa480ef16"],["/posts/3312011324.html","eb6ef2012b85ef3b1ab3c9029a1c4a9c"],["/posts/336911618.html","cf7a46420a7147d84e096fb448d2b64a"],["/posts/3402121571.html","cb44bb1e8a538d4f401c258b1d1e2b32"],["/posts/3405577485.html","4ed59e2079c5ce61aa6b6174b23db0fd"],["/posts/3498516849.html","bdb05b1f513d42d6acc58e9fb9edc6a1"],["/posts/350679531.html","59af5b3339be129a99e1d60cc0fdecc9"],["/posts/3513711414.html","1fdff370568d4288e99321aaf6a78f32"],["/posts/3523095624.html","0319f397b3af81259a7842de9561651b"],["/posts/3546711884.html","f93cdb7281371ece3af96c1deda5f2cd"],["/posts/362397694.html","d617df806e556b3a9062ce4c1da9fd4c"],["/posts/3731385230.html","5fe6903612e8e5adece35201d6c87fcc"],["/posts/3772089482.html","e03f311200e1786faee76ebef8b29ffd"],["/posts/386609427.html","a968745be9ad82aa6d58b02a45cdaa73"],["/posts/4044235327.html","917708553969734874a4a0d4b327d69b"],["/posts/4115971639.html","755d78b9f09ab478dd02f0697b751146"],["/posts/4130790367.html","5798e4ab1aefcf470949652a0da1f46e"],["/posts/4131986683.html","bfd301e6fe637e6482e47cbb0a1c8fdc"],["/posts/4177218757.html","a2ef3f8c1f3a1d5dc9526600bf760de7"],["/posts/4192183953.html","bff64f12eaed52867d599668a8628dc1"],["/posts/4223662913.html","e47fb227bf8df09cd4764566ed967776"],["/posts/4261103898.html","b10a2f686665133cf70fb0bb72550787"],["/posts/4286605504.html","4f1091d1338a6edab3ac23e911541148"],["/posts/449089913.html","bc53a0d16258b718145f35692ae6a67e"],["/posts/469711973.html","c32c08eea5d99940005285154fe5368b"],["/posts/482495853.html","0f9d7743407157dcabb0f7b73637b68f"],["/posts/488247922.html","1489596e0d19753b0ec839c4374265e9"],["/posts/517302816.html","d2c562cb6bf154955aa1119591f5ca58"],["/posts/570165348.html","b1097ba2474aa04c45d22834e0d3eb12"],["/posts/595890772.html","94d7d0c01f7a197b91e0437ad6727b7d"],["/posts/67485572.html","cf6ab0829904175aeadb72538061c325"],["/posts/694347442.html","029052eda7b5195fd68755a50adaede4"],["/posts/707384687.html","c5afc17cde030ee9aae0cf6ddc3a8e62"],["/posts/71180092.html","4d1c392aa50875da94f95e5f0193b64b"],["/posts/716459272.html","b9f4f19951e097cc430ba1d10d7b8e22"],["/posts/765481613.html","608484726544c656ebb048492db116f0"],["/posts/778231993.html","c41a82795fa6b149bff8b05d17c592be"],["/posts/795397410.html","01d7ac7a724c65d6cb38c5967261482a"],["/posts/820223701.html","b68c589312b5a8c046e71f5c0bbeff61"],["/posts/830372185.html","4fee8430571f086f2dc43f17db723356"],["/posts/88294277.html","e6f867f66ae7ca751a7f5ce9df43c567"],["/posts/939963535.html","3887ee09ebb28f7ffd3cc9d04398fb25"],["/posts/983786067.html","fab570cb6db4579c556c22c8f1fa139e"],["/sw-register.js","627d04c9c79cb2aad79ed9914b8be3e7"],["/tags/C/index.html","706ba7ccfe76ad57faaab28e54d31ba1"],["/tags/C/page/2/index.html","7d22576b245ec4046ff062dd07ce4a79"],["/tags/C/page/3/index.html","9f407056e1b2a1eea44fd7977289733f"],["/tags/C/page/4/index.html","2a98926107dbd44dedb5c4100d7d3d5a"],["/tags/ETL/index.html","ddf0575eb33e78c2b7cd4ebec454a271"],["/tags/ElasticSearch/index.html","d21dd2b2c59a2718229cd9e6f6203ca1"],["/tags/GUI/index.html","ec9fe27e06283eaab3586b5857d1a299"],["/tags/HBase/index.html","5dadd76602886d3f56fc104619ee9cb5"],["/tags/Hadoop/index.html","7745f8d88431d85cd76a97976cd56493"],["/tags/Hadoop/page/2/index.html","b4ed8f48481db812417bfb2d5eaaf99f"],["/tags/Java/index.html","d2e9005778955cb9a51a3fd4e67601ea"],["/tags/Java后端/index.html","dd1f70a527f9d71bf38f58088a0031fe"],["/tags/Java后端/page/2/index.html","f31abd1371c5c50d9fbcb0b981880970"],["/tags/Java基础/index.html","5a7d5a40840719bac5de7ed34a5b11a2"],["/tags/Java基础/page/2/index.html","9f6166f4dd7be8d8be37a63882226a6a"],["/tags/Kettle/index.html","f8d5b894806cb2927ddd1fab50457a97"],["/tags/Kibana/index.html","277b1cfc9e92354c98e9f670e298b343"],["/tags/Linux/index.html","87e859ae22273a466861a840763070b2"],["/tags/Linux/page/2/index.html","10dde68d65add82197ed66508f00a80d"],["/tags/Linux/page/3/index.html","434840c98e289ab94848242df79856f8"],["/tags/Mac/index.html","7a796d4624d860e2b6ebd8b4d41afae1"],["/tags/Mac/page/2/index.html","c7712c0ba3d7aeeb496406849144fddf"],["/tags/Maven/index.html","3feba7cf4d3e9baadd18c121f129ecb9"],["/tags/MySQL/index.html","20b7430c81a241ba018a246d57b6bbd7"],["/tags/Python/index.html","878a77115ce6c8e6e682481baf6e80e4"],["/tags/Redis/index.html","d9136c2817c51c11c87e80b07ebb39ca"],["/tags/R语言/index.html","be1a3d5afbacb4580d3827c955304a67"],["/tags/Spark/index.html","f6bcb785ee3dc06c1c1c3c13e42c55da"],["/tags/Ubuntu/index.html","77939d0cbdfdff3fe6e4f44751a1476c"],["/tags/Vue/index.html","a3ef28f37094e01c17383454edcb4bfe"],["/tags/Windows/index.html","725ab652d5b6644e6281674a4099c906"],["/tags/ZooKeeper/index.html","6dc8a28942a416e4361d73b1ae0ca49b"],["/tags/bfs/index.html","42a76cb87150cce98525e2c7b24b92e5"],["/tags/dfs/index.html","15ff51787c3fb0a60beb7ac456f7b2e0"],["/tags/folium/index.html","70c7f4987dadb922f67be6e1ec7c4e3f"],["/tags/git/index.html","579b27bdbd0f539c54a8bed9d22e1a92"],["/tags/iPad找电子书/index.html","9f798391261f418b1dd6ada1eb4f84f9"],["/tags/index.html","a2517d906da4ddb9f7ed406c8413fc5b"],["/tags/latex/index.html","6219885bf0f84dbc3a041ff2664fc07d"],["/tags/中间件/index.html","abf40baa293038c5b7225e74c1fd978e"],["/tags/二分查找/index.html","8dd40d104cb45d3ccf39de6366871a35"],["/tags/优化类/index.html","c8f17d55de230a8bfa4d867c058a49c9"],["/tags/前端/index.html","5ff553e1590c587d6942056435e4d94e"],["/tags/前缀和与差分/index.html","009553416f6cb355a726dd15b1aa75bc"],["/tags/动态规划/index.html","664dac7d3583422962ce9a18f8d3abce"],["/tags/动态规划/page/2/index.html","aa86c47b372022dee7fbcd8ab3f28b0f"],["/tags/博客搭建/index.html","4a85ffac2cbfabf060dad95138f428dc"],["/tags/图论/index.html","07f0176ef036963fecf1b9719511b86d"],["/tags/大数据/index.html","ef9cea80068122d384c4521a4d3ae7e3"],["/tags/大数据/page/2/index.html","55a9243ad51baba92b5997d585e490e4"],["/tags/排序/index.html","d8c02146530a767a3f3ef3f89f0e7f11"],["/tags/操作系统/index.html","95c3ab2dd71c46b6fb03bc4debbc8b1c"],["/tags/数学建模/index.html","fe9b1a6565e1ffeba3559722681ac293"],["/tags/数据库/index.html","7857376f81fab134ed723ac62ed13412"],["/tags/数据结构和算法/index.html","fd5d993c28b80a6a4103b32efdc6aff1"],["/tags/数据结构和算法/page/2/index.html","d491fd3c369ce450f49428594ab4c213"],["/tags/数据结构和算法/page/3/index.html","b7047bd618374c03fe19bca6e0c0118c"],["/tags/数据结构和算法/page/4/index.html","e514d80cd9e22a161aaad89fbcbdf5f9"],["/tags/数据结构和算法/page/5/index.html","2311c2a181dc79009cb59682598624b7"],["/tags/数组和字符串/index.html","4ee9d91c9c3ed212bd06f0cf53f3a4fc"],["/tags/数论/index.html","94cc69033c6f90faa6f7a007f77de3a8"],["/tags/枚举类/index.html","c2bc9c653987ff4b3464ce2c1f9a0a1e"],["/tags/栈和队列/index.html","74c16df02c5818e25d474d2a74a3dd49"],["/tags/树论/index.html","3d1dea9118dffc7b73d65dfe8283f1e4"],["/tags/测试/index.html","24c44239a3c15752f1f19b98df2935a9"],["/tags/环境/index.html","bd13f027fb885ea5c60ef8ce0a01f4fe"],["/tags/环境变量/index.html","b6c489b2b100b06d964431a745bd65ef"],["/tags/绘图/index.html","cd7fac1155888efb0a0c62c65ab35e1a"],["/tags/编程工具/index.html","20d07e7beb445a5bc05c91e6a722a9b6"],["/tags/编程环境/index.html","9f431cd61f39d404e588485c4c042b5f"],["/tags/网络编程/index.html","081ddea7f9b3281eef8fb6fddea43a3c"],["/tags/英语语法/index.html","4ede35b98cb01cc118a5ff24981bde96"],["/tags/计算机操作系统/index.html","3acc53b83addef1edfcab45c8ff8dd6a"],["/tags/论文/index.html","361e79e52a29003e23e5569a1fb6e3fc"],["/tags/资源下载/index.html","43f6640100e3372e284a333808fc7142"],["/tags/链表/index.html","583661ffa9b7215d81fa165abf309172"],["/tags/集合/index.html","4f52f62e5dd5031a0c3ad556d0a71d8a"],["/tags/集群/index.html","164f76cb7c1bd0daf5ea9909d3a46afc"]];
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
