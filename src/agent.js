var env = {
  getNumStates: function() { return 7 * 5 + 1; },
  getMaxNumActions: function() { return 4; },
};

var spec = {
  num_hidden_units: 200,
  experience_add_every: 2,
  learning_steps_per_iteration: 10,
  experience_size: 20000,
  alpha: 0.01,
  epsilon: 1.0,
  gamma: 0.99 // minimal discounting
};
var agent = new RL.DQNAgent(env, spec);

var app = Elm.Gridworld.fullscreen();

app.ports.agentAct.subscribe(function(args) {
  // TODO use destructuring
  var field = args[0];
  var alreadyRewarded = args[1];
  var state = field.concat([alreadyRewarded]);
  var action = agent.act(state);
  app.ports.agentMoveBot.send(action);
});

app.ports.agentLearn.subscribe(agent.learn);
