
% Steven Maere, 2025
%
% This script fits a piecewise linear model to nuclear cross-sectional area
% data (in pixels). The piecewise linear model consists of 4 horizontal line pieces
% representing ploidy levels 2C, 4C, 8C and 16C. Next to the ploidy transition boundaries, 
% the average 2C nuclear cross-sectional area and a ploidy scaling factor
% (i.e. the factor by which the average cross-sectional area scales as ploidy doubles) 
% are estimated from the data. The model is fit using 100 repeats of
% particle swarm optimization.
% 

x = (1:1:22)' ;
data = NaN(8,22,10);

% Col hair cell nuclear cross-sectional area data (in pixels) without last cell,
% columns are cell files, rows are cells from position -13 to 8 relative to
% the first elongating cell (0). Last cells in all cell types were ignored 
% because they exhibit unusually low nuclear cros-sectional areas
% indicating imaging bias.
data(1,:,:) = [56	50	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN;
58	47	43	NaN	87	45	NaN	72	58	59;
63	64	60	NaN	94	67	80	68	70	96;
40	52	65	55	102	NaN	40	78	66	73;
56	51	49	68	54	62	60	79	74	95;
68	78	49	55	67	80	68	121	76	62;
64	66	55	76	81	78	74	81	79	57;
80	67	45	58	67	86	102	99	48	65;
75	99	59	92	87	96	56	100	69	91;
69	71	64	85	69	147	47	107	117	57;
97	76	65	68	57	116	104	119	104	71;
100	75	83	75	73	118	68	99	83	74;
90	76	96	57	118	127	95	63	102	78;
71	108	94	60	171	149	78	177	78	55;
136	102	157	110	240	138	98	156	155	130;
170	166	114	185	278	203	66	175	206	125;
150	247	241	177	240	222	89	135	220	134;
203	228	231	218	344	270	136	165	193	124;
253	226	457	258	392	256	77	154	279	158;
353	162	353	434	206	330	175	169	319	187;
284	171	240	360	281	255	108	321	289	172;
220	178	373	NaN	NaN	292	140	NaN	NaN	NaN];
%NaN	140	123	66	138	128	85	176	84	114];

% Col non-hair cell nuclear cross-sectional area data without last cell
data(2,:,:) = [NaN	40	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN;
29	39	NaN	NaN	NaN	NaN	62	NaN	NaN	69;
35	50	NaN	45	NaN	76	67	48	69	54;
45	44	63	57	NaN	63	71	45	62	71;
24	54	45	50	76	63	46	68	72	84;
28	62	45	47	83	73	52	64	60	46;
70	46	48	45	78	54	51	56	72	49;
67	55	42	60	66	63	65	81	62	62;
74	65	55	70	93	42	63	80	61	63;
84	70	74	58	57	50	60	65	65	88;
60	50	74	36	124	73	95	81	145	121;
73	73	92	57	139	100	89	95	77	89;
88	52	88	84	100	91	56	72	103	97;
72	61	79	88	129	135	85	90	103	124;
68	80	82	102	NaN	105	148	105	85	148;
71	77	115	121	229	119	119	133	115	178;
83	105	164	150	155	136	142	153	NaN	153;
113	124	94	141	331	152	217	161	NaN	161;
104	191	151	146	NaN	277	190	154	NaN	111;
139	162	NaN	196	NaN	303	200	196	NaN	NaN;
104	119	191	203	NaN	NaN	NaN	257	NaN	NaN;
129	NaN	245	191	NaN	NaN	NaN	NaN	NaN	NaN];
%177	NaN	91	110	100	80	61	91	106	73];

% sim hair cell nuclear cross-sectional area data without last cell
data(3,:,:) = [40	61	45	58	53	57	75	36	58	47;
47	49	65	46	51	79	81	47	71	48;
48	74	80	69	50	74	84	45	87	43;
33	44	58	94	68	36	64	60	43	45;
59	33	53	62	58	42	61	65	53	46;
52	43	41	63	61	106	63	71	43	46;
55	32	58	68	94	58	48	80	67	110;
77	39	51	61	72	66	67	68	60	63;
72	53	66	61	80	71	62	62	60	59;
59	44	45	66	63	74	68	86	56	154;
75	63	62	81	78	67	77	117	73	77;
66	42	65	84	97	87	116	87	62	142;
75	67	50	63	93	81	77	91	98	76;
66	84	51	79	97	113	70	103	119	84;
161	97	152	109	90	172	137	183	173	214;
135	125	105	136	184	173	118	161	160	186;
146	172	112	86	144	229	214	163	105	138;
226	163	120	122	159	212	279	204	116	162;
235	134	154	236	146	117	215	249	155	184;
269	132	138	121	145	NaN	NaN	NaN	169	168;
268	197	147	106	171	NaN	NaN	NaN	NaN	NaN;
227	250	114	138	NaN	NaN	NaN	NaN	NaN	NaN];
%214	195	133	NaN	NaN	NaN	NaN	NaN	NaN	NaN];

% sim non-hair cell nuclear cross-sectional area data without last cell
data(4,:,:) = [21	35	57	40	47	40	56	NaN	55	46;
51	39	46	38	41	47	68	NaN	59	54;
28	49	49	38	50	35	66	NaN	83	53;
40	43	42	43	61	50	56	NaN	67	58;
45	49	55	45	56	65	67	59	52	46;
73	47	60	37	63	66	92	61	62	48;
55	40	58	36	57	64	66	64	75	43;
60	47	56	56	55	65	58	67	103	59;
50	59	63	44	58	70	59	78	64	52;
88	46	58	115	80	49	63	86	107	59;
48	31	66	64	71	41	91	88	55	60;
62	28	95	92	67	68	89	116	46	68;
62	50	77	99	74	75	106	95	96	77;
47	62	76	155	90	66	81	103	114	82;
60	54	147	92	75	66	93	101	111	125;
52	86	93	85	114	65	143	125	95	NaN;
66	69	75	109	92	73	219	91	97	165;
60	78	81	97	88	51	175	192	91	145;
92	94	123	145	91	132	262	148	92	NaN;
107	222	119	NaN	99	210	NaN	NaN	120	NaN;
73	379	121	NaN	154	135	NaN	NaN	103	NaN;
176	240	NaN	NaN	214	229	NaN	NaN	NaN	NaN];
%126	193	NaN	NaN	NaN	NaN	NaN	NaN	NaN	NaN];

% ccs52a1 hair cell nuclear cross-sectional area data without last cell (only 8 cell files)
data(5,1:22,1:8) = [38	68	55	46	29	74	83	72;
42	59	70	67	32	74	53	63;
52	66	56	48	54	81	59	87;
50	69	38	70	60	83	60	76;
85	59	59	68	43	74	101	119;
66	73	66	63	39	76	114	86;
80	75	89	55	94	65	99	97;
109	74	80	79	80	97	136	83;
71	76	85	65	106	98	111	114;
87	92	84	50	128	151	118	114;
85	97	83	68	135	74	215	125;
83	101	82	94	121	60	141	115;
89	201	99	87	98	84	107	86;
136	99	67	69	106	93	228	102;
92	182	83	141	103	207	137	190;
97	170	270	77	127	119	260	121;
94	152	183	153	195	135	123	107;
232	175	159	154	172	224	288	102;
169	166	263	194	128	230	105	118;
255	NaN	184	130	144	NaN	NaN	231;
215	NaN	144	128	139	NaN	NaN	185;
158	NaN	NaN	160	139	NaN	NaN	NaN];
%196	NaN	NaN	NaN	158	NaN	NaN	NaN];

% ccs52a1 non-hair cell nuclear cross-sectional area data without last cell
% (only 8 cell files, one missing value on row 17 replaced by NaN)
data(6,1:22,1:8) = [50	57	34	NaN	48	49	26	40;
46	66	34	NaN	49	43	36	56;
42	59	59	44	43	48	41	67;
54	45	59	44	68	48	48	81;
54	82	60	46	36	58	45	74;
55	45	52	26	47	67	53	67;
54	47	82	49	65	69	50	73;
60	57	85	70	49	51	59	101;
53	47	102	64	76	68	69	63;
48	60	94	84	53	48	49	77;
71	48	48	63	77	68	94	64;
47	58	52	69	62	55	106	99;
47	85	115	57	66	76	92	97;
69	64	87	93	82	84	83	91;
79	72	113	56	106	185	93	108;
91	70	52	112	131	180	131	148;
78	64	111	64	NaN	153	115	304;
146	81	111	98	120	73	69	278;
105	69	154	86	NaN	217	115	427;
142	65	95	130	NaN	240	85	125;
250	132	119	NaN	NaN	NaN	125	NaN;
NaN	132	NaN	NaN	NaN	NaN	NaN	NaN];
%NaN	157	NaN	NaN	NaN	NaN	NaN	NaN];

% sim ccs52a1 hair cell nuclear cross-sectional area data without last cell
data(7,:,:) = [48	52	37	42	NaN	59	61	53	46	25;
51	49	49	46	30	68	53	42	49	23;
50	51	42	88	34	66	38	61	74	28;
74	56	72	59	34	56	38	77	60	33;
68	65	34	74	52	39	51	71	52	30;
56	59	36	57	66	39	47	53	60	38;
52	75	52	66	69	61	51	90	41	54;
69	70	40	52	68	72	79	66	43	60;
67	114	39	61	67	63	98	78	61	43;
130	126	43	70	91	63	75	92	63	50;
154	63	47	54	90	78	65	113	72	59;
58	63	41	62	78	49	87	76	54	56;
76	89	49	92	110	59	59	102	76	49;
169	90	49	83	119	68	73	131	110	59;
195	122	48	73	123	141	145	96	107	93;
119	190	56	92	141	141	164	172	101	77;
186	217	70	86	87	110	165	252	108	127;
200	164	44	94	145	81	218	133	105	119;
183	179	74	65	126	127	158	314	127	118;
NaN	180	64	104	97	104	114	145	130	233;
NaN	NaN	119	102	132	202	NaN	NaN	95	278;
NaN	NaN	125	108	132	NaN	NaN	NaN	122	73];
%NaN	NaN	145	NaN	184	NaN	NaN	NaN	80	NaN];

% sim ccs52a1 non-hair cell nuclear cross-sectional area data without last cell
data(8,:,:) = [26	45	23	62	21	NaN	31	33	NaN	NaN;
28	66	24	66	30	35	49	39	21	26;
41	57	30	60	25	41	42	42	28	37;
37	47	40	44	31	47	37	67	30	42;
68	54	47	56	35	63	65	40	45	38;
61	92	53	50	26	52	48	40	43	36;
61	61	44	51	42	89	53	38	54	52;
80	50	62	51	36	63	72	45	48	58;
77	59	69	66	38	48	50	40	56	39;
86	76	64	54	44	49	65	35	52	45;
73	78	51	67	88	60	112	59	52	54;
93	111	90	53	67	72	110	53	48	57;
49	73	57	61	131	51	78	68	45	58;
58	69	59	63	107	53	92	95	46	72;
84	87	91	77	93	76	92	91	152	76;
90	93	65	48	62	103	90	120	111	76;
141	69	54	43	130	92	200	NaN	109	120;
87	98	59	69	112	106	136	116	96	121;
93	99	77	76	77	114	145	162	216	101;
101	72	47	63	75	72	NaN	NaN	348	147;
102	NaN	54	69	51	94	NaN	NaN	320	236;
83	NaN	109	84	NaN	103	NaN	NaN	99	NaN];
%NaN	NaN	NaN	77	110	NaN	NaN	NaN	NaN	NaN];

I = find(~isnan(data));
rankeddata = sort(data(I),"ascend");

options = optimoptions('particleswarm','SwarmSize',1000,'MaxStallIterations',50,'HybridFcn',@fmincon);

for(i=1:1:500)
    i
    % first parameter : 2C average cross-sectional area, constrained to range of observed cross-sectional areas
    % second parameter : ploidy scaling factor, constrained to [1, 2.5] (1.58740105 = spherical nuclei, 2 = cylindrical nuclei under assumption nuclear volume scales with ploidy)
    % other parameters : triples of 2C, 4C and 16C boundaries for each of 8 cell types
    % As 16C is only observed experimentally in H cells in the imaged root tip zone, N cells were constrained to only exhibit up to 8C ploidy levels
    [par(i,:),fval(i),exitflag,output,points] = particleswarm(@(prm)linfit(data, prm), 26,[rankeddata(1),1,0,0,0,0,0,23,0,0,0,0,0,23,0,0,0,0,0,23,0,0,0,0,0,23],[rankeddata(size(rankeddata,1)),2.5,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23],options);
end

[f,ind]=min(fval);
optimpar = par(ind,:);

figure(1); clf(1);
titles = ["WT H","WT N","{\it sim} H","{\it sim} N","{\it ccs52a1} H","{\it ccs52a1} N","{\it sim ccs52a1} H","{\it sim ccs52a1} N"];

% for each cell type
for(i=0:1:7)
    subplot(4,2,i+1);    
    
    % extract relevant optimized parameters
    par2 = [optimpar(1) optimpar(2) optimpar(3*i+3:3*i+5)];
    cell=[];
    cell(:,:) = data(i+1,:,:);
    av_cell=[];
    stdev_cell=[];
    
    % delete last 2 cell files 9 and 10 for ccs52a1 H and N because no data
    if((i==4)||(i==5)) 
        cell(:,10) = [];
        cell(:,9) = [];
    end
    
    for j=1:1:size(cell,1)
        av_cell(j) = mean(cell(j,~isnan(cell(j,:))));
        stdev_cell(j) = std(cell(j,~isnan(cell(j,:))));
    end
    
    % first and last cells often have fewer data points restricting
    % stdev, e.g. first stdev for control atrichoblasts is 0 and last (22nd)
    % stdev for ccs52a1 atrichoblasts is 0 because there is only 1 data point, causing trouble in optimization. 
    % Put all stdevs for cell numbers with <4 data points to stdevs of following cell (for first cell) or preceding cell (for last cell). 
    
    if(sum(~isnan(cell(1,:)))<4)
        stdev_cell(1) = stdev_cell(2);
    end
    
    if(sum(~isnan(cell(size(av_cell,2),:)))<4)
        stdev_cell(size(av_cell,2)) = stdev_cell(size(av_cell,2)-1);
    end
    
    [fit,bnd1,bnd2,bnd3] = piecewiseLine(x, par2);
    
    plot(x,av_cell+stdev_cell,'k');
    hold on;
    plot(x,av_cell-stdev_cell,'k');
    plot(x,av_cell,'ko');
    line([1 bnd1],[par2(1) par2(1)],'Color',[0 0 1],'Linewidth',2);
    line([bnd1 bnd2],[par2(2)*par2(1) par2(2)*par2(1)],'Color',[0.2 0.6 1],'Linewidth',2);
    line([bnd2 bnd3],[(par2(2)^2)*par2(1) (par2(2)^2)*par2(1)],'Color',[4.666666666666667e-01 6.745098039215687e-01 1.882352941176471e-01],'Linewidth',2);
    line([bnd3 22],[(par2(2)^3)*par2(1) (par2(2)^3)*par2(1)],'Color',[6.352941176470588e-01 7.843137254901961e-02 1.843137254901961e-01],'Linewidth',2);
    
    if(bnd1 ~= 1)
        line([bnd1 bnd1],[0 par2(2)*par2(1)],'Color','black','LineStyle','--');
        text(bnd1+0.25,30,int2str(bnd1-14),'Color','r','FontSize',12);
    end
    line([bnd2 bnd2],[0 (par2(2)^2)*par2(1)],'Color','black','LineStyle','--');
    text(bnd2+0.25,30,int2str(bnd2-14),'Color','r','FontSize',12);
    if(bnd3 ~= size(cell,1))
        line([bnd3 bnd3],[0 (par2(2)^3)*par2(1)],'Color','black','LineStyle','--');
        text(bnd3+0.25,30,int2str(bnd3-14),'Color','r','FontSize',12);
    end

    xlim([1 22]);
    xticks(1:1:22);
    xticklabels({'-13','-12','-11','-10','-9','-8','-7','-6','-5','-4','-3','-2','-1','0','1','2','3','4','5','6','7','8'});
    ylim([0 rankeddata(size(rankeddata,1))]);
    hold off;
    title(titles(i+1));
    xlabel('Cell number');
    ylabel('Nuclear CS area (pixels)');
    
    %waitforbuttonpress;
end



