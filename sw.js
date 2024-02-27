/**
 * 自动引入模板，在原有 sw-precache 插件默认模板基础上做的二次开发
 *
 * 因为是自定导入的模板，项目一旦生成，不支持随 sw-precache 的版本自动升级。
 * 可以到 Lavas 官网下载 basic 模板内获取最新模板进行替换
 *
 */

/* eslint-disable */

'use strict';

var precacheConfig = [["/404.html","aa26417c10ff923e4ea4576978d469c2"],["/about/index.html","48eb0e0cb5a79b14691a22d5075eb79f"],["/archives/2023/01/index.html","845974fb399d7b100b2cc9b52c1669f9"],["/archives/2023/02/index.html","b41cdbfa0be7a574faa3bd6e3cf7fbae"],["/archives/2023/02/page/2/index.html","1538c962d7c03db23e80ed4de3f33299"],["/archives/2023/02/page/3/index.html","d55a0167e6a60917e92db271e03898ec"],["/archives/2023/03/index.html","7cbda3a1c3ad66b94b028fb35a4a0693"],["/archives/2023/05/index.html","e289eb63584b45c616e934afb8b17fe7"],["/archives/2023/06/index.html","549a4c755bb2cf2573a1f02c0d408d42"],["/archives/2023/09/index.html","ce46e7dfc623785662eacde7634b07a6"],["/archives/2023/11/index.html","ca665f5df888c5493e36887e07ccb1ba"],["/archives/2023/12/index.html","d33e4d345e0cedbf981d25c0a4dbda10"],["/archives/2023/index.html","425cd2e3ea5617cf712eb5c52c9353df"],["/archives/2023/page/2/index.html","4db02577a997b7a034775375b258109c"],["/archives/2023/page/3/index.html","a29f580ae9a74ef43ad0060e7baf5f79"],["/archives/2023/page/4/index.html","9e7a11625e83c56f64d476ce5b82a5b4"],["/archives/2023/page/5/index.html","7ec75bea2b2861ba9d235298fa9f344d"],["/archives/2024/02/index.html","ffebe5ee7b292a7ab2a359300cda4562"],["/archives/2024/index.html","e218001bb0b6f4a1b63a3857df656ed8"],["/archives/index.html","bac11a0ff65c05a840fa2d095e2b6796"],["/archives/page/2/index.html","bff9396ed54cd77e8f6ac456a2cd508e"],["/archives/page/3/index.html","58a2ac57cefb32122207330956a588cd"],["/archives/page/4/index.html","769ae2fd462c3e73f356fe024388b471"],["/archives/page/5/index.html","d0604113b37ce9e23b9ca3e5a9436dc4"],["/baidu_verify_codeva-qQP2iZOMLX.html","23667762e780c95c9df5223b7870b1e5"],["/categories/Java/index.html","e855f9c5f2c9c3475044dae61ab9d20c"],["/categories/Java/后端/index.html","8f12d5052c62ddb67a947b0de678a887"],["/categories/Java/基础/index.html","9fb2af34fafbc241e0923326de836a73"],["/categories/Java/基础/集合/index.html","e343af1c4abf0d54d8c7dce503f392ee"],["/categories/Python/index.html","bcbe0e846023f7ba1fedc232737664a3"],["/categories/Python/编程环境/index.html","4144fdaa1bd20477e5ff1820e591199f"],["/categories/R语言/index.html","b057c4e0fbadb56ff392b15e77acc024"],["/categories/R语言/编程环境/index.html","c6fc25b80258ebc1bb959a59d654ffae"],["/categories/iPad/index.html","430892e1ad79a8c7fadbbc09a2460732"],["/categories/index.html","52a4d40eb6f0217b4aa4400c3cc00e84"],["/categories/中间件/index.html","cda1dd21300cee85ecdf6cf5275265f3"],["/categories/前端/Vue/index.html","a182162b40db9bee749a85a4c9e27302"],["/categories/前端/index.html","74b75f3139e3ca8c7f3c2513a64737bb"],["/categories/大数据开发/ElasticSearch/index.html","2e3dfaa9fe5cda55067e007523f8917b"],["/categories/大数据开发/ElasticSearch/环境搭建/index.html","8b460f4d51425f5aa0e3f6e4b498a6e9"],["/categories/大数据开发/HBase/index.html","a7b411fad34da57c402390b9a2eef4ed"],["/categories/大数据开发/HBase/学习笔记/index.html","33c0a222ffc2b9e00e2a967ade24ea41"],["/categories/大数据开发/HBase/环境搭建/index.html","2d64f9efd29b3ea1d8a48f5831f7466e"],["/categories/大数据开发/Hadoop/index.html","1e01416ec048bea932c031fe45b18658"],["/categories/大数据开发/Hadoop/技术/index.html","5cf5bd4c39b9ef7160441d03b3f2e3d1"],["/categories/大数据开发/Hadoop/环境搭建/index.html","3a14a96f3a5e7a10de626ceeeb89b779"],["/categories/大数据开发/Redis/index.html","3e9ed41a6379f9b57f82fc6b6728c509"],["/categories/大数据开发/Redis/技术/index.html","1f2ade6d8b1a19e543d3744fe2d665af"],["/categories/大数据开发/Redis/环境搭建/index.html","d4ba21ae9bf970c2430cbd978c29c4b1"],["/categories/大数据开发/Spark/index.html","9b57784bf402a1cec00ab6c0dd0b5528"],["/categories/大数据开发/Spark/环境搭建/index.html","a0581f860942d8cb9d217ad579e9c944"],["/categories/大数据开发/Zookeeper/index.html","475e7d8746263c473f7ba83232b72dc9"],["/categories/大数据开发/Zookeeper/环境搭建/index.html","58e9ad18fe80d2da57bc13d301547537"],["/categories/大数据开发/index.html","e6e7a8c513777b4451c6f5d08d580cbc"],["/categories/学校课程/index.html","5b942a42199bdee36f263c6d6f622ef3"],["/categories/学校课程/计算机操作系统/index.html","29e0588b0eab4595b13fe8c21b0f8c42"],["/categories/操作系统/Linux/index.html","05e86edd3dc50605f2ab43df0a30b42a"],["/categories/操作系统/Mac/index.html","5a4d79dcaafb7e31b8cde71d97c52fcc"],["/categories/操作系统/Windows/index.html","117e6f350271a8459853ee9eeebc42f5"],["/categories/操作系统/index.html","b4b47a3e7e308a0fb17af13f528692ca"],["/categories/数学建模/index.html","0006a747f50d42430b3def294e97bfa9"],["/categories/数学建模/latex/index.html","87b04a06f6992a32bf91d436ec6b140c"],["/categories/数学建模/优化类/index.html","32abf58ee5e1c8beabcb2328dd6b92bb"],["/categories/数学建模/优化类/现代优化算法/index.html","eadb31b3d2cbaf5b6dbe850340831ea1"],["/categories/数学建模/优化类/规划类/index.html","f0982fb32d7eb5e78bf609fca52cef56"],["/categories/数学建模/绘图/index.html","f501c6a9bac135003918afa8bafd5886"],["/categories/数据库/MySQL/index.html","12974157e25123e68a96cba2ae7b76b6"],["/categories/数据库/index.html","10fb2553068fc13a2e3ec69c31f631e7"],["/categories/数据结构和算法/index.html","81dce692086e91fc40b1c842ec16fe0e"],["/categories/数据结构和算法/page/2/index.html","4f63513fd00f516d6c4f4799cd634211"],["/categories/数据结构和算法/基本原理/bfs/index.html","4e2a0888608cd56357a4009d0b3b435c"],["/categories/数据结构和算法/基本原理/dfs/index.html","8726dd20708171e5c9917c5578ac70af"],["/categories/数据结构和算法/基本原理/index.html","8cbe779ad9197dc0d290538a9534937f"],["/categories/数据结构和算法/基本原理/动态规划/index.html","c39d8c3ce27d63e51690297864c8aa2b"],["/categories/数据结构和算法/基本原理/动态规划/背包类/index.html","329ad5af59308b26e599645242ba6352"],["/categories/数据结构和算法/基本原理/图论/index.html","9a799568b32689a0a695b69849f1575d"],["/categories/数据结构和算法/基本原理/字符串/index.html","320e44e8a2cd96e127872ad30ced7a77"],["/categories/数据结构和算法/基本原理/排序/index.html","8e057ee4fe591edee1051843e30a7c27"],["/categories/数据结构和算法/基本原理/数组和字符串/index.html","d9ac1f918f0217771d95a0cce8eebb1b"],["/categories/数据结构和算法/基本原理/数论/index.html","ba50b5d490f2017ebf792a4f0c870b8d"],["/categories/数据结构和算法/基本原理/树论/index.html","48ac34f309947bc56b9a35012e32cb01"],["/categories/数据结构和算法/基本原理/链表/index.html","23cb85fb9a099d21c1ff0b6c01717011"],["/categories/数据结构和算法/算法题/index.html","ee6f1872f7af438325f83bad658534da"],["/categories/数据结构和算法/算法题/二分查找/index.html","e576f3acbbaaade45dff797e3f938aa1"],["/categories/数据结构和算法/算法题/前缀和与差分/index.html","2b15f9d3b98be0da062d024e28610b7d"],["/categories/数据结构和算法/算法题/动态规划/index.html","3041bd5ce25aeb8393f3295966038cbd"],["/categories/数据结构和算法/算法题/动态规划/线性dp/index.html","428f591f889c85ec2a876b1c1e27dcc6"],["/categories/数据结构和算法/算法题/动态规划/路径类/index.html","cd002541b103006bbf874b52a8ec04db"],["/categories/数据结构和算法/算法题/动态规划/递推类/index.html","49961ba7befb44873ff62b1415e86b9b"],["/categories/数据结构和算法/算法题/数组和字符串/index.html","2a114a722f03e1a9913cc1bb37cb7c03"],["/categories/数据结构和算法/算法题/数论/index.html","7c16af17a52a4dc7affe486b5a39dcc8"],["/categories/数据结构和算法/算法题/栈和队列/index.html","64dfeca711385eabb38eed9a811d0a25"],["/categories/数据结构和算法/算法题/树论/index.html","2d67943c1bb40185a1bb5369a7a6523c"],["/categories/杂七杂八/index.html","d33196d2326e4f796c2ec940fbeac379"],["/categories/杂七杂八/博客搭建/index.html","9dd5c44882e2e56213486f236e1817a8"],["/categories/编程工具下载/index.html","9ab51d38915a47dec24331854ee1210b"],["/categories/编程环境/index.html","42a34ac3325a9d7463251fe12c32214f"],["/categories/编程环境/大数据/index.html","3cc3b0793e8ca4802b780c91570afd6b"],["/categories/英语学习/index.html","0517bd3f792c0ba55dcbe46723599691"],["/categories/英语学习/英语语法/index.html","69b566c4f3da6858b9e94027b966ac69"],["/comments/index.html","6678999e254a0832bed04751df5ea131"],["/css/artical_side_transport.css","7a364dbbfd1f8f0ab707b4bad21e15de"],["/css/background.css","ac15c4d880156c18b725de616496838e"],["/css/footer1.css","fe60b76038a770bee3d0959f36e7108f"],["/css/footer_bg.css","732a1860a4f0f1a59e03f8b08bde26a3"],["/css/index.css","26e5a9c5c533e1365a6aea3548bc859c"],["/css/maoboli.css","3a429c198e3d7460fcf39fc5f1318164"],["/css/nav1.css","b6a5ff17066bff20d1e451115f4c6332"],["/css/twikoo1.css","2a1d73918cc55cf55aa7f889f51192ec"],["/css/twikoo2.css","0577747a2a9555baefc708447feabfad"],["/css/var.css","d41d8cd98f00b204e9800998ecf8427e"],["/img/404.jpg","4ef3cfb882b6dd4128da4c8745e9a507"],["/img/favicon.png","7a8c47cb5a2149c1a1af21e90ecd9ca7"],["/img/friend_404.gif","68af0be9d22722e74665ef44dd532ba8"],["/index.html","2efe3fb08e496347a5da6b7e0eb37587"],["/js/countup.js","ac5341fdcb5757d947af5b44539ce708"],["/js/main.js","88aced9e00713346dbf5f92453a349b2"],["/js/search/algolia.js","786b8da5325888c55c04e6b6687bf9f5"],["/js/search/local-search.js","aea55acb22a3b51ad16057b0ea52c0a9"],["/js/tw_cn.js","bc064917c366036544975274bb20a01d"],["/js/utils.js","e95ad73d5170f72ae1596b3d9abb7ed3"],["/link/index.html","cc544d30230aebf4b0c6000ed19894ef"],["/movies/index.html","99581e239a937be33ee9673c7354813b"],["/music/index.html","605f966e463524ac1c8f3b512bc00fe3"],["/page/2/index.html","1e7f825361d177787ab137519813dc76"],["/page/3/index.html","e19176a4f468e549633d4847dcbc9913"],["/page/4/index.html","5aa0110c0e83738ae4e71341380d74ff"],["/page/5/index.html","d92049d460723543b62c07520d2d9f7d"],["/page/6/index.html","2340e3c39fc55927fbb92703c782dc81"],["/page/7/index.html","8f6cfb74d80a9dd723961954d85ae355"],["/posts/1021360842.html","d9205168bb9a545ad26570a7c670650b"],["/posts/1120620192.html","4698d41af41280ae176c4ffdb2c59a5e"],["/posts/1137707673.html","11357902a6b51f90a53bd06019afc6a1"],["/posts/1141628095.html","e70bc62edd195387716970693172b5fc"],["/posts/1168613674.html","1277987a7b71264777639362e99d9d76"],["/posts/1219920510.html","e2c22f9f82549f80bd8f5be565e05520"],["/posts/1222166338.html","10679e1b39b9454ce1ae5563ea1ecf1b"],["/posts/1259097482.html","1f8039293cc1a172674b289bb3a50074"],["/posts/1271036369.html","45492bedbd05e393eae9979215621bf8"],["/posts/1312847445.html","76225ab8e40813b74bc3dd9428e9c610"],["/posts/135355774.html","c19a5ceba4715bf17d70c42bf26de63d"],["/posts/1375344716.html","02666c807f33fdf915a654158b47b831"],["/posts/1388991698.html","0533d43b2a5264c1a688748300c5dbc6"],["/posts/1410315814.html","8e811a6f214982ffd5afdd689692f533"],["/posts/1452790229.html","b15fd0f103816d87174f4690d6c443fb"],["/posts/1470079884.html","0440222832baabee5f709bc1aa36f822"],["/posts/1470079885.html","585059bb6b1230d25176b7b19afc6fa7"],["/posts/1470079886.html","1b0687ba6f8092f06c43e46b510c69a7"],["/posts/1470079887.html","0119ba0a7cc9f15c7e7e67bcb95ea8e7"],["/posts/1498536549.html","eee42d85fdebc6d2d6395a2f3db70ecd"],["/posts/1539568593.html","a4e676e1826bda49e0e56cdc6831d188"],["/posts/1547067935.html","6953746dc3b2c590447f51e003497708"],["/posts/1557866301.html","f7b53699a315e06f531d4938a82a8b78"],["/posts/1571776361.html","13b0e42a835c3ff721435861f9b33239"],["/posts/1605124548.html","162894345440d4d5c0ed036e32e5e5d7"],["/posts/1633036852.html","21921e90064ffc8fd97bb18894f48641"],["/posts/1667740714.html","a83add5eef2e933a7df0e5b7e204c204"],["/posts/1674202625.html","0d972f101926ad6b1a3098b458f02c78"],["/posts/1765123828.html","3acbf92879d91de3231976da2c93101b"],["/posts/1767336200.html","e16f44c54a0a0304c3a2e5fa47bf350c"],["/posts/1776114197.html","55a10635e39b57ead27362b43b77216a"],["/posts/1817748743.html","af36e6296653763aad95dbbcabc73b6e"],["/posts/1925125395.html","f3709e11e7c59fea6373ef260b6226d2"],["/posts/1966191251.html","88b2a74a692dfdd2809dcaa261b9bcf5"],["/posts/1987617322.html","05d93799f44acaafa1933d0f315fcbf7"],["/posts/1999788039.html","1c9394d84487884979fb8964f98e8573"],["/posts/2075104059.html","4bea614fb6ba04a825d333a453c28847"],["/posts/2087796737.html","91b1af13dbafd357751e568643959daf"],["/posts/2106547339.html","2a8b7591e9c35159469c346127e3c49b"],["/posts/2207806286.html","3be11c07bcecc09324960217d0ccb712"],["/posts/2225903441.html","3e7e93358434bdcd92a1428b6f29a05f"],["/posts/2265610284.html","da3c8f3223b8745db52bd1c9f7c0a4a5"],["/posts/2281352001.html","19adbd7885e98daa77c731a042e36f3e"],["/posts/2364755265.html","6a78bf90b1453b634f2026c1d7fe8f3a"],["/posts/2414116852.html","dccc504855480dd4eb1551fcecd4b4dc"],["/posts/2421785022.html","81e9d8973c86574f5c74867263a581d4"],["/posts/2482902029.html","0ed01cd0507791354b93e53afa38b0c4"],["/posts/2495386210.html","a806848a643c75d8cce378540679aece"],["/posts/2516528882.html","bf45f51ee828e73c551cfcd737defb16"],["/posts/2522177458.html","32566a3f9262b5b4eff17c19f29a607c"],["/posts/2526659543.html","27990d9af748acc1781698a5e9c98f0b"],["/posts/2529807823.html","ea4f771eeb562aefb0c41741bba489a3"],["/posts/2596601004.html","74ba1f4bf084b277fdd3169801ce2af0"],["/posts/2697614349.html","ce7d06c003052551d9827c1e48446f36"],["/posts/2742438348.html","72db9b963bbd8de06c063a7be1aeff2c"],["/posts/2768249503.html","9d0d1a3bf2e0ac419632ee7768e15442"],["/posts/2864584994.html","358e4d5133d5b07d92694e46783084ff"],["/posts/2888309600.html","af916a5ee174f6271edb330d5c47b9ac"],["/posts/2891591958.html","787fc3d4ce5ed0ba4c92406bd48d05f7"],["/posts/2909934084.html","07de873f5ffbc9a841ae504e61d54d33"],["/posts/2920256992.html","862c6e5f81b928b17c40b58a4b9a29c0"],["/posts/2959474469.html","38bc4359999cd5d5d70fadfb79b5eb9f"],["/posts/3005926051.html","d189f0d764cb382b31c29b7ec982ecaf"],["/posts/309775400.html","3729e8ca0aceb606ae3727d9074d704f"],["/posts/3156194925.html","64ea652e1e450ce469ca4a4d00583f16"],["/posts/3169224211.html","a09c69faa775ce0328317976253c76e3"],["/posts/3213899550.html","3bfa8c12a3cc8280c414a753abf454de"],["/posts/3259212833.html","32b7ea051b3ce67efb2d809140588f92"],["/posts/3265658309.html","9c99d45ab804411b5f9a26ca096458c7"],["/posts/3266130344.html","ccda2bec95d7779520e5d7771b069b8e"],["/posts/3292663995.html","32b93c4ac8721ae4dc9a811498a8a22e"],["/posts/3297135020.html","5eb70c6621c61cba51d759c9433d2bb7"],["/posts/3306641566.html","11b476f73c8abb035ae8de31a5e08fd4"],["/posts/3312011324.html","031563370197cb9a292d6114182d69c3"],["/posts/336911618.html","46276d6dc06d6c0c999cc00bf84d48c5"],["/posts/3402121571.html","6d207694946f3680d8d58bc78aeb6040"],["/posts/3405577485.html","b5675de6229943e671c8246f29aab523"],["/posts/3498516849.html","775e12f1e35a9056dd1a2ec3ba26b123"],["/posts/350679531.html","aa7a1500182812a16d53eeaab657e99f"],["/posts/3513711414.html","8d40db39bbdb9ca787ce4c5d043073a7"],["/posts/3523095624.html","206c9bfe2aa02166bbc18d555e2aadfb"],["/posts/3546711884.html","2b6ad0cea718b5e4b9c755c2b42db772"],["/posts/362397694.html","1bc8d53119d6f6b2d717621cd322cfde"],["/posts/3731385230.html","63ade47ba6ce42538de0c8afcb91d790"],["/posts/3772089482.html","66cf5898e2d3985d3c3266daf0166b9f"],["/posts/386609427.html","ccc17066c5a937fd98b3e6bce29b7041"],["/posts/4044235327.html","cbb92921f9743e77364fc52cffd98188"],["/posts/4115971639.html","8109f054f322da32f94f61a3d1cc6954"],["/posts/4130790367.html","3c336434520dfdc61690842253a8572c"],["/posts/4131986683.html","23d630ac384fb123398f5960a6c47c9b"],["/posts/4177218757.html","b66a9bccf0d77756e68756e716e10e60"],["/posts/4192183953.html","aef1cf7c15be0c603ba0025e7660424a"],["/posts/4223662913.html","3a59da65c7b634584e03bbd2876937d0"],["/posts/4261103898.html","788ff9dcc923b21bbaf259b3ebd42188"],["/posts/4286605504.html","943eae574fd607535e4e8a0873fe1d11"],["/posts/449089913.html","c12fb6cb6e20ac811c8169d0da850d1e"],["/posts/469711973.html","0a412ef9be2dd8c0f282fcdf991666c4"],["/posts/482495853.html","b6ce7c05a435dc4fc4af6dd882ca483b"],["/posts/488247922.html","52d70521453b86dc06b0258b70b558f0"],["/posts/517302816.html","548bfd541236f1f3de111cf9b8fcd13b"],["/posts/570165348.html","2a737855da47fef998091de9c4502da3"],["/posts/595890772.html","965288ef65a4c176ad0702b57c317d86"],["/posts/67485572.html","28c0078e6df6d6c3a47084a5981c2f49"],["/posts/694347442.html","004fc86105c90ffd4f2212b3f33023aa"],["/posts/707384687.html","3ec92529e984f56d8d4e558bad67e45f"],["/posts/71180092.html","ee8eafcd0d4bfeb8d0282ca5f540071a"],["/posts/716459272.html","dec1a5c46b338aecd0a43f98ca372c02"],["/posts/765481613.html","07204fab4bf5ddfb93a2ef6ae56f0544"],["/posts/778231993.html","db7d729457b4b90c54a98293080742dc"],["/posts/795397410.html","7ca98d7168b7ccc0fa26f2d11d046729"],["/posts/820223701.html","68180a7db99a6d119bfe08349c23c021"],["/posts/830372185.html","9e3fbe69887193ccfdc6569e839fab0a"],["/posts/88294277.html","296d66c7d09825d108266519468e5ad7"],["/posts/939963535.html","d8c546e21de96132edb1d4d0ddbf59e4"],["/posts/983786067.html","9461e411c7124eccdaf880b799038bb4"],["/sw-register.js","547b6c94b6be3161836665946be901c2"],["/tags/C/index.html","b7fc74aadc6e84e9573301dabf1e2b13"],["/tags/C/page/2/index.html","f14e352c342b93fbc9646488de472e1d"],["/tags/C/page/3/index.html","6e9d2ad75d7d48884dccbb03b8faffd3"],["/tags/C/page/4/index.html","e43924420315a3eb0e4d9b6ef5cb1d93"],["/tags/ETL/index.html","37e42dcc4bd4258e20461a111c3e33e4"],["/tags/ElasticSearch/index.html","63f62fa8902c7d2c094a7ae3081962b6"],["/tags/GUI/index.html","b1d6373ab6275dcd8a942e44d456fdb9"],["/tags/HBase/index.html","0e46c33551380ce918f89820b91a8076"],["/tags/Hadoop/index.html","6d816b07a4df3751021146702ed20bc3"],["/tags/Hadoop/page/2/index.html","5d014f7356d69f7b2d26195112907c98"],["/tags/Java/index.html","f33d27e7fc330e81359130dd0fc15a00"],["/tags/Java后端/index.html","f6d47c7cfc753028541363e36a8967ae"],["/tags/Java后端/page/2/index.html","f6d1f7dfc4817a2c8d36f4df9f2d73ce"],["/tags/Java基础/index.html","5a1b4e3b2a4055b5913c4bc8bf0641d1"],["/tags/Java基础/page/2/index.html","2d6806da141f33edef9b2fd4bfa514d7"],["/tags/Kettle/index.html","807fee8a6ee874fab93391eb41866e0e"],["/tags/Kibana/index.html","6ef65c5b6e1cecd2668e8588b8b6763b"],["/tags/Linux/index.html","ac6e7895b36e29de69975dfcc083fd89"],["/tags/Linux/page/2/index.html","e479f86d06c68ba8b4f3fc108ed0d414"],["/tags/Linux/page/3/index.html","17d3b210ed8b4d1e08312359a32c7db6"],["/tags/Mac/index.html","0e5d055f2f5ac4665b7683d76cead78a"],["/tags/Mac/page/2/index.html","17a6a9b754d8ccbcec65590911221735"],["/tags/Maven/index.html","4c0ef6245513ed5f6c83d78cc265d5ca"],["/tags/MySQL/index.html","8078d52fab7313c3d45e1a42c074f480"],["/tags/Python/index.html","913fffd66492c8cc0c5b8e73b8cf4106"],["/tags/Redis/index.html","72a9d3c80814a79778e575e6a5407894"],["/tags/R语言/index.html","956e44a5d8e855a2fa7b655dc258a9f6"],["/tags/Spark/index.html","314a351866b123455d1aa35c3f884ccd"],["/tags/Ubuntu/index.html","046c88c68fc3bd6b0fe744c0fdda0227"],["/tags/Vue/index.html","3b82608b94df70674f4759eb324d685b"],["/tags/Windows/index.html","604745b502ff576171a6a77eca838748"],["/tags/ZooKeeper/index.html","ede539da3d9d781028d2ca367dac0376"],["/tags/bfs/index.html","9bf37c759902dac7ba32504fd5b94d3a"],["/tags/dfs/index.html","94559479dcc4359df8c4cc19ffe01c26"],["/tags/folium/index.html","2441e1b98948c777dd489fac58a4a983"],["/tags/git/index.html","d231bbf9d843c731874cdbf9c6d02b3d"],["/tags/iPad找电子书/index.html","5581ba4433e76b38a25df1bb2ece855c"],["/tags/index.html","62a2a0a0edeb115fd06d612127285b4b"],["/tags/latex/index.html","565207d7533ff05a616ba54de852dbdf"],["/tags/中间件/index.html","9d90a6463b46d0776cdb97a0a0b314d3"],["/tags/二分查找/index.html","ac63f8c059a64eb35e90aae3554c6485"],["/tags/优化类/index.html","c71009ba69b96185cf18755c5689dfe4"],["/tags/前端/index.html","751f6c8e35d20569912e08641790b97d"],["/tags/前缀和与差分/index.html","4520b78be61c2709b0292eee354a9fbb"],["/tags/动态规划/index.html","d95663996235cb4b9b14e1bee9ecdf3d"],["/tags/动态规划/page/2/index.html","552b639071614dcf6d36df7bea66d92b"],["/tags/博客搭建/index.html","36d5354e8eaebb71d6ba52dfc65eedc8"],["/tags/图论/index.html","1c49b26d8704ed9148d886d52f4d470e"],["/tags/大数据/index.html","465b07474402d15354564a4741c01813"],["/tags/大数据/page/2/index.html","f3e6f7519fe9cdf3bc1bce55f18b2ec8"],["/tags/排序/index.html","b5308d87e3e09a6a482951b553abf63e"],["/tags/操作系统/index.html","e93cbdc5133cf8e907e8e47eb2ae8c07"],["/tags/数学建模/index.html","4a02497b218a0ed2c53f76771d1fc220"],["/tags/数据库/index.html","c11f24b2321f1382aaab89614b6cd471"],["/tags/数据结构和算法/index.html","1951421bb0bb662b0bb2e79beccc4385"],["/tags/数据结构和算法/page/2/index.html","0e3b04efeae3cff92d0a71e51478027a"],["/tags/数据结构和算法/page/3/index.html","a394e09ca82c7b4c05e5acd3fdfc98f9"],["/tags/数据结构和算法/page/4/index.html","292e2739aad03e5dcf9b8845e32e504e"],["/tags/数据结构和算法/page/5/index.html","1a5e247359b17d1b5df06cc1588be14f"],["/tags/数组和字符串/index.html","59e37bd1f4eac1b93533965a0dd0a176"],["/tags/数论/index.html","56f80a7bd1fd8f65c6f349ae4057aa8e"],["/tags/枚举类/index.html","d47adcf653d2151a3c2e9d0300958955"],["/tags/栈和队列/index.html","51bd1b62b22f1bfbb0dce7af715df80c"],["/tags/树论/index.html","a65ffafb91fcc94852d0b682dd707a55"],["/tags/测试/index.html","9e1afbbc27628f918a50119766d9f091"],["/tags/环境/index.html","c18aec685330b3771271c142196f3e6b"],["/tags/环境变量/index.html","bb4403bf4e38474aa7a872c95c8656b0"],["/tags/绘图/index.html","f8ca520d9e9f8264e40598b827b7bd87"],["/tags/编程工具/index.html","836d987f90475e181663cc7e76229909"],["/tags/编程环境/index.html","bbf14ba292578eddad3651146e4108cc"],["/tags/网络编程/index.html","f940cf8a026cdd78e00561926e4d1379"],["/tags/英语语法/index.html","6274c720ee6e0cf80d78150ae52cb21d"],["/tags/计算机操作系统/index.html","cc9b329698c6c524190d17e8e6e05bbc"],["/tags/论文/index.html","b676f67094eb2a8ef225d3342b44c16e"],["/tags/资源下载/index.html","ca9470450dca35fd48c1d6825018d306"],["/tags/链表/index.html","fc38ba1aaccb5104f4c9959a8d510ff3"],["/tags/集合/index.html","982240276bd4d3a020f2889df837fa4a"],["/tags/集群/index.html","17ceb26893589b9daa47db28ff002b3b"]];
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
