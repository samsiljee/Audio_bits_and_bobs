---
output:
  pdf_document: default
  html_document: default
---
# Mass spectrometry sonification
### Sam Siljee - 18th March 2024
I wanted to represent the data in a non-visual format.
Graphics are a logical way and clear way of presenting this data, however I wanted to explore other methods.
Sonification is not a new methodology, however I feel like the data from mass spectrometry is particularly well suited to sonification.

## Tone synthesis
Sonification has been done before with scientific data, most commonly as a dry or simplistic plugging of a multiple series of numbers into a synthesiser to produce an odd string of synthetic piano sounds.
I really wanted the data to be fairly represented and honoured by the produced sound.
Hence I chose to use only a minimal intervention when tonifying the data.
The only synthetic process involved is generating sine waves from the data.
I have also avoided any transformation of the data.
I noticed that the m/z values observed happen to be in the audible range when converted directly to Hz.
The actual process of tonification is straight-forward.
Each spectrum is taken in isolation, and a set of sine waves are generated, one for each peak of the spectrum.
The m/z value is mapped to frequency of the sine wave, and the intensity of the peak is mapped to amplitude of the sine wave.
These sine waves are then summed to produce more complex waveforms and the characteristic tone for each spectrum.
Tones are clipped to a default 1 second.
There is an interesting symmetry in that one of the detectors used in the mass spectrometer for some acquisition methods, the orbitrap, does essentially the opposite process.
The orbitrap takes a set of spinning ions in a magnetic field and detects a complex waveform from which the Fourier transformation is used to resolve groups of ions with very high resolution.

### Tone synthesis variations
Four things to play with with tone generation; 
1. Filtering out low intensity peaks
2. Scaling linearly (especially useful if the m/z values fall outside of the audible range).
Human hearing generally falls between 100Hz and 15kHz.
3. Log transformation of the m/z scale (human hearing over pitch is logarithmic, not linear)
4. Revese m/z values - this is intuitive as this gives heavier ions a lower pitch.

An observation I've made is that when setting the linear scaling values high, beyond human hearing, you can stil generate audible sound.
Interestingly this sound stays audible and similar as you increase the scale values higher and higher.
Note that this is only the case if the ratio of scaling minimum and maximum values stay consistant.
This indicates the the sound that we are hearing is an emergent interference pattern, rather than the sine waves themselves.
In science, an observation that stays consistant even under changing conditions is deemed to reflect an underlying truth.
Does this mean that the audible interference patterns are what the data really, truely sounds like?
Does this have implications for conventional mass-spectrometry data analysis?
I suppose it's already in use, as at least in proteomics MS2 order scans we look at the intervals between peaks more-so than absolute m/z values.
Another interesting observation is that the tones very very slowly change in dynamics over time.
Unfortunately the computation memory and time required with my current code is very large, so I haven't been able to generate tones longer than a minute or so, but there are subtle shifts in the beats.
This is more apparent in the more complex spectra, but unfortunately those are also more complex to synthesise!
It may well be worth working out a new method, perhaps by writing every sinewave to a .txt file before combining in order to compute longer tones for more complex spectra.

## Piece synthesis
The first test data, "1e6_phospho_converted.mzML" is from a test of sample extraction, phosphopeptide enrichment, and methods of running the mass spectrometer in July 2022.
The orbitrap was used for acquiring this data, resulting in fewer total spectra, but higher-quality with higher accuracy and resolution. 
The dataset contains a total of 58,248 spectra, of which 4,517 and MS1, and 53731 are MS2.
The acquisition method was 90 minutes, and hence the complete piece is also 90 minutes long.

## Background noise
In science, a commonly used term is background-to-noise ratio.
Out of the signal intensity received, how much carries "useful" information as opposed to random noise or technical artifact.
This is analogous to how turning up the volume on the stereo can increase the signal, but not improve the audio quality or improve the signal to noise ratio.
One of the questions I often run into when explaining my work to non-scientists is why I discredit certain observations as "artefact".
In my familiarity with techniques, I have developed the skills to recognise the difference between meaningful signal, and meaningless blotches on the microscope slide, or 'random' peaks in data.
To me, I discard these observations without a second thought, but to someone observing without the same pattern recognition as myself, it seems like arbitrary dismissal of valid data.
Depending on the technique, making the call between artefact and signal can be straight forward, however I have run into a problem with this sonification project.
A particular issue is the click heard at the start of every clip.
The origin of this click is an artefact of the process of creating the tones, all of the sine waves initially start synchronised, before moving out-of-phase.
This very brief period where all the waves are synchronised manifests as a high amplitude click.
The problem with this, is that in the volume normalisation process, this high-amplitude lead-in compresses the rest of the waveform, loosing volume.
On further reflection however, this click may serve a purpose.
In the MS1 channel it helps to give a sense of rhythm to the piece.
In the MS2 channel it gives a "gramophone record" crackle, which is artificially added to give texture in certain electronic music genres, although this is generally not to my liking.

## Artefact, artifact, artifice
In science, artefact is somethign which is meaningless, in fact it is outright unwanted as it obscures the truth that scientists are searching for.
It is interesting therefore that in art, artifact (note the different spelling) is described as something of value.
This is an interesting exploration in this project.
For example, the click at the start of the sonification (mentioned elsewhere in the essay also), which I have chosen to leave in (for now).
This mirrors the artifice I was describing in some electronic music of an artificially added vinyl crackle.
Other artefacts are genuinely less helpful however, when computing images for the image method visualisation, spectrum number 7950 in the test dataset has only one peak - thus causing my code to crash during the synthesis of the images.

## Process of interpretation
13th of March - In discussion, I came to realise that this process represents an interpretation of my data.
It is a process of taking machine-intelligible sets of millions of datapoints, and transforming it into something that a human can grasp. 
Despite artificial intelligence, machines and humans do not think alike.
This is why programming languages are needed to translate from one world of comprehension to another.
The standard approach to interpreting mass-spectrometry data, is to first statistically process the data to clean it up and summarise the many datapoints into biologically more interpretable summaries.
Firstly the spectra are matched to protein fragments (peptides) from a reference database.
These matched spectra are summarised to those that match the same peptides (protein fragments), these are then further summarised into peptides from the same proteins.
Further interpretation is all about providing context, where samples from different groups are contrasted to look at relative differences.
These proteins which are identified to be different are then referenced against known biological pathways to get biological meaning.

One lesson which has become apparent to me during my PhD study is that science can be limited by the tools available to us.
A classic example is the thought that sequencing the human genome would unlock the mysteries of life, and cure all diseases.
We now know better.
Part of the challenge of this process has been when people ask me why I did this. The easy answer is that I did this for fun, which is true.
However, when I first started on this project I felt an enormous drive to progress with this project.
Perhaps what this project represents is an early attempt to push the boundaries of how we understand mass spectrometry data.
Of course it's very difficult to extract any biologically useful information from the produced soundscapes, and therefore very challenging to demonstrate any practical usefulness of this project.
The main exception to this is that with practice, the soundscapes could arguably be used for quality control of the raw data, and of course it opens the raw data up to visually impaired researchers.

## Challenges
- Coding process
- Computing time - days of computation on a standard desktop computer to sonify a single mass spectrometry run.
An experiment can involve hundreds or thousands of runs.
- Musicality and aesthetics vs letting the data speak for itself.
Objectivity vs subjectivity.
There are ways of artificially coercing data into rhythms and harmonies.
- Dynamic range - Should I use logarithmic scaling of total ion current for tone loudness?
- Click at the triggering of each clip, this is an artifact of all of the sineswave starting in sync, initially some big spikes in the waveform before they move out-of-phase.
- How literal to be
- (Not yet tested), it reflects technicalities of the aquisition method more than biological differences at this point
- The balance in art between challenge and approachability.

## Wish-list
- Deal with the clicking when clips are triggered
- Use other header data to produce envelops for the sounds, attack, duration, decay etc.
- I'd like to integrate processed data from PSMs in some way also - this with give more values to work with to form envelops for example.
- Visual accompanyment
- Invert m/z values? Heavier ions for lower tones?
The MS2 scans show more variety, with the MS1 scans starting off with the machine droning of background noise before the peptides start to enter the mass spectrometer.
At around the 18 minute mark the MS1 channel starts to sound more tonal, with tones waxing and waning as the corresponding peptides elute from the column.
The rhythms in the MS1 scan are initially faster, then slow down as the cycles include progressively more MS2 spectra.
The fixed rhythm is determined by the cycle time of the mass spectrometer acquisition method.
As mentioned previously, background noise is a feature of the produced sound which distorts its clarity, a challenge all scientific observation methods struggle with.
As with standard interpretation, differing peptides are not necessarily distinguishable from the MS1 spectra, presenting as a single tone.
The diversity of the MS2 tones, tinkling away, is what differentiates the peptides and allows us to identify them.
Where the MS1 sounds are repeated as the same peptides are sampled over the course of the run, the MS2 sounds are not repeated - this is a feature of the acquisition method, the dynamic exclusion list, where peaks from the MS1 spectra are excluded from further MS2 analysis after the firs analysis.
This is to free up time for other less intense peaks to be selected for analysis.
In summary I suspect that the sounds heard reflect acquisition methods more so than biological features of the data.

## Tonification web tool
In discussion with Neil, we came up with a variety of data manipulations to explore the data further.
Namely:
1. Reverse the m/z values, currently heavier ions, counter-intuitively, give rise to higher-pitched frequencies
2. Scale the values to the range heard by human hearing.
Currently the m/z values are the same as the scan range set during in the mass spectrometer method, roughly 200 to 2000 m/z.
These can be scaled from 100Hz to 15kHz for example.
3. Log scale transformation of the m/z values.
Human hearing is logarithmic, the data is collected on a linear scale.
4. Filtering out the low intensity peaks.

One solution in R to "trying lots of things out" is to develop an interactive Shiny application.
Accessibility is also important to me, and most people I discuss this project with have no coding experience.
They are missing out on the fun of playing around with the data and seeing what happens.
A Shiny application can also be easily published as a website, allowing greater access to the process.
One interesting discovery I made during this process, is that when the data is scaled well beyond the range of human hearing, there is still sigficant audio signal produced in the range of human hearing.
I believe this to be due to the fact that there are so many peaks in my spectra, many of which have very similar m/z values, that interference patterns are a significant source of the produced audio.
These interference patterns will of course persist even when the frequencies of the underlying sine waves are too high to be heard.
This is one of the benefits of interactive coding!

## Visualisation
I?ve had several people ask me about adding a visual component to the work.
I think this is a good idea, especially as the same principles used to generate the sound can be used to generate visuals.
Advice from Matthijs, the screen saver aesthetic has been done to death, and switch people off.
I'm much better off making something mechanical, automaton like.

I have generated two different (basic) visualisations for the NEB competiton entry. In the "plot method" I have plotted the MS2 scans and corresponding MS1 scan back-to-back, with the axis rotated 90 degrees. I've also mapped the total ion current of the spectra to the contrast of the plots. The second method, the "image method" has the MS2 spectra scaled as horizontal lines, and the corresponding MS1 spectrum mapped as vertical lines. The intensity of the peaks is presented as intensity.

In my attempt to decide which of the two methods to present for the competition, I circulated the two videos amoungst my friends and family. There were some interesting observations. Firstly, a lot of people were convinced that the sounds were different (I used the same 2 minute audio clip for both), this was echoed in my discussion with Darrell who works as a VJ, and he agrees that visual accompaniment can significantly alter the perception of sound. He also warned me strong visualisations can easily distract and draw the attention away from the audio, which is the real aim of this project.

Another interesting obersvation is that my scientific friends gravitated towards the plotting method, which is graphically more striking, but also much easier to interpret as a literal graphic representation of the data. SOme criticism of this method include that it was too obvious. The image method was generally preferred by my aquaintances in the arts, who preferred the increased abstraction, and found it more elegant.

### Automata
Ideas for automata: 
- 2D array of rods with adjustable height.
Bin the peaks, and adjust heights as a histogram.
Have the retention time scroll across the other dimension of the array.
- 2D array of rods with the first row representing MS1 spectrum, and the 20 other rows representing the top 20 MS2 associated with that MS1.
This way there?s far less movement in the system, as they only have to refresh once per cycle time period.
- The rods can be covered in a cloth to smooth the peaks, this would also add a screen for illumination if needed.
- Bouncing light of the surface of a wave pool, or two mirrors mounted to two speakers - MS1 for x, and MS2 for y

## Installation
Another potential to develop this project is as an installation.

### The shape of the space
The outside of the space could be presented as a black box, both a literal representation of the black box of the mass spectrometer, and a representation of mysterious processes occuring inside.
The entrance to the installation is like the ion source of the mass spectrometer.
My intial idea was to have the space as a cone, at the smallest end where you cant even crawl into it I would have individual peaks on individual spectra playing individual sine waves.
Subsequently there'd be a larger space where you can squeeze in a playing whole tones from individual spectra, then bigger space where you can play whole runs.
A larger space would be about comparing runs and experimental groups.
Realistically howver mass spectrometry is done in discrete stages and steps, so more like rooms bracnhing off into ever smaller rooms.
This may also help flow through the exhibit, however then questions arrise as to how restrictive/claustrophobic the smallest of rooms should be. There will also be concerns for safety in the event of an emergency.
The rooms can be floating on stils up orr the floor, with space to crawl out underneath.
Every room can split into two smaller rooms with a binary decision, in fact the entry room can literally be named "0", dividing into "00" and "01", with "01" dividing into "010" and "011" for example.
I imagine there being one entrance room, splitting up into 8 smallest rooms. Each of these smallest rooms will have their own diorama.
After the smallest rooms, they could recombine sequentially till there is only one exit room.
Personally, I'm struggling to make these binary decisions.
Should I pursue science or medicine? Academia or research? Medicine or surgery?
The exhibition rooms ultimately come back to a single room, and ultimately all of our decisions come to one conclusion - we die.

I imagine the lighting starting off as either white or black, and splitting up into narrower and narrower ranges of the spectrum as yuo get into the smaller rooms.
I imagine the smallest room being lit by a single lightblub of a single colour, each of the eight having their own portion of the light spectrum.

The largest space where you can explore a taxonomy graphic representation of contexts of the project.
Alternatively the context map could be on the outside of the space.

The smallest space could also be represented by a diorama which only one person can peep into at a time, containing a collection of the smallest details.
The dioramas can all be the same in each of the smallest rooms, however the fact that they are all lit by a single and different light means that each will have a different component of the diorama visible.

### Diorama
Things to represent in the diorama:
- ions
- magnetic fields
- physics
- stuff too small to see
- stuff too small to understand
- stuff I don't get
- "censored values"
- "below the limit of detection""

### Printouts
Another striking visual to potentially include in an installation would be to printout all of the thousands of spectra in the project.
This is of course unrealistic and incredibly wasteful of paper, however if could still be represented by a handfull of spectra being printed out, with piles/boxes of unopened packets of printing paper.
There could also be an option to print out a spectrum for yourself, and then to see how your own manual annotation contrasts to automated computer annotation.
Comparing and contrasting human and machine elements could be an interesting topic to explore with this project.
What can you do by hand?
When do you have to resort to computers?
the answer to this largely comes down to scale.

### Hierachy of scale/context map/taxonomy
biology/lung cancer >
lung cancer biology >
scc of lung >
research of biology >
basic science >
my PhD (in an of itself a tiny sliver of information)>
cell culture model >
cells >
proteomics >
protein extract >
purified peptides >
mass spec experiment >
run >
protein >
peptide >
PSM >
spectrum >
peaks >
ions

## Miscelaneous
- Part of the code came to me in a dream, I woke up up the answer to something I'd been puzzeling over for weeks - namely how to combine multiple tones into a sonification of the entire run in R.
I literally woke up one day with the solution in my mind.
In my mind the code was a simple 3 lines, however when I came to actually writing the code it ended up as about 30 lines.
Goes to show that even code bends reality and expectations in dreams.
- The aspect that has most interested people less familiar with mass spectrometery that I've spoken to about this project has been sense of scale.
What is big picture? what is detail?
- Why did I change to writing the essay on github? Version control?
- How much should the sounds in discrete sections be isolated from each other?
Should they be kept pure?
Should they mingle together into a larger piece?
- Art and science have traditionally been seen as separate. (See NEB reference)
However, this distinction is not naturally occurring.
Young children do not see this difference, indeed it is only while progressing through school and university that areas of interest must be progressively narrowed.
This is one of the things I value most from my Steiner education, holding art alongside science.
Both are methods to explore and understand the world around us.

## Acknowledgements
- Margie
- Matthijs
- Julia
- Charlie
- Sanne
- Hanne
- Clara
- Neil

## References and resources
1.	Gulland A. The neuroscientist formerly known as Prince's audio engineer. Nature [Internet]. 2024 Mar 14 [cited 2024 Mar 19]; Available from: https://www.nature.com/articles/d41586-024-00791-5
2. https://mlaetsc.hcommons.org/2023/01/18/data-sonification-for-beginners/
3. 10.21785/icad2023.4039
4. Gibson, J. (2020, October 21). Bacterial Art - incubating creativity in the lab | NEB. NEBinspired Blog. https://www.neb.com/en-nz/nebinspired-blog/bacterial-art---incubating-creativity-in-the-lab
5. https://koenderks.github.io/aRtsy/
6. https://journals.sagepub.com/doi/full/10.1177/20592043221080965
7. https://doi.org/10.1121/10.0011549
8. https://doi.org/10.1016/j.proeng.2012.04.095
9. Hermann, T., Hunt, A., & Neuhoff, J. (2011). The Sonification Handbook (Thomas Hermann, A. Hunt, & J. G. Neuhoff (eds.)). Logos Verlag. http://sonification.de/handbook
10. Sturm, B. L. (2005). Pulse of an Ocean: Sonification of Ocean Buoy Data. Leonardo, 38(2), 143–149. https://doi.org/10.1162/0024094053722453 - Paper using similar sonifcation methodology to my own. Also, Leonardo may be a suitable journal for publication of this work.
11. https://www.watershed.co.uk/studio/
12. https://www.nature.com/articles/d41586-024-02197-9
13. https://complexity.wgtn.ac.nz/
14. https://pubs.aip.org/asa/jasa/article/151/6/3685/2838268/A-method-to-convert-neural-signals-into-sound
15. https://icad.org/
16. https://www.youtube.com/watch?v=6Tc7XI0iUYU
17. https://www.youtube.com/watch?v=3JzpLsLCaog
18. https://www.youtube.com/watch?v=9IdVyArDlZ4
19. https://www.youtube.com/watch?v=yT7K4MaOBDA
20. https://www.youtube.com/watch?v=aOA_wAoh5go

## Notes
- What is the more universal message I'm trying to communicate with this work?
- What new perspective do I want to bring? What assumption do I want to challenge?
- What is my conversation with the data?
- Scale and complexity, being engulfed by it, fractal nature, interactive and engaging
- Big picture vs detail, my Phd is about tiny narrow specific field, detail gives information but big picture/context gives meaning.
- Mass spectrometry takes a big biological question, breaks physical samples down to the point where they are pulled apart and distilled to nothing but numbers.
These numbers are then used to build up a picture again. This is similar to the alchemical process, where a spygyric preparation breaks a plant apart and distills every constituent part, then combines every part again into a holistic preparation.
- Analysis is an iterative process, would it be worthwhile circulating through the exhibit again?
Taking the understanding from a previous cycle forward into understanding a new cycle?
- The tendency is to go into a science experiment rational - head first, then to come out experiential - with empirical evidence.
The reducing entrance chambers to the installation could have no sound in them, with the sound only building up from the smallest of chambers.
- I'm not the first to use R for art, art from various geneartive algorithms has been implemented in the aRtsy package.
- People are most interested in the story behind the data, where does the data come from, why this data, what it the process of transformation. People want to see how the magic is done!
- Emergent properties; this is a feature of complexity. I believe that the sounds are largely emergent properties, as is evident by the audio produced when the peaks are scaled well outside of the range of human hearing. In another sense, this whole project can be seen as an emergent property of my PhD project.
