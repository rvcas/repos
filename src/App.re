type webData('a) = RemoteData.t('a, string);

type repo = {
  id: int,
  owner: string,
  name: string,
  full_name: string,
  stars: int,
  html_url: string,
  description: option(string),
  fork: bool,
};

type action =
  | ChangeUsername(string)
  | SearchEnterKeyDown
  | Loading
  | ReposLoaded(list(repo))
  | ReposError(string);

type state = {
  username: string,
  user: string,
  repos: webData(list(repo)),
};

let forkPath = "M8 1a1.993 1.993 0 0 0-1 3.72V6L5 8 3 6V4.72A1.993 1.993 0 0 0 2 1a1.993 1.993 0 0 0-1 3.72V6.5l3 3v1.78A1.993 1.993 0 0 0 5 15a1.993 1.993 0 0 0 1-3.72V9.5l3-3V4.72A1.993 1.993 0 0 0 8 1zM2 4.2C1.34 4.2.8 3.65.8 3c0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3 10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2zm3-10c-.66 0-1.2-.55-1.2-1.2 0-.65.55-1.2 1.2-1.2.65 0 1.2.55 1.2 1.2 0 .65-.55 1.2-1.2 1.2z";

let svgPath = "M15.2 40.6c-.2 0-.4-.1-.6-.2-.4-.3-.5-.7-.4-1.1l3.9-12-10.2-7.5c-.4-.3-.5-.7-.4-1.1s.5-.7 1-.7h12.7L25 5.9c.1-.4.5-.7 1-.7s.8.3 1 .7L30.9 18h12.7c.4 0 .8.2 1 .6s0 .9-.4 1.1L34 27.1l3.9 12c.1.4 0 .9-.4 1.1s-.8.3-1.2 0L26 33l-10.2 7.4c-.2.1-.4.2-.6.2zM26 30.7c.2 0 .4.1.6.2l8.3 6.1-3.2-9.8c-.1-.4 0-.9.4-1.1l8.3-6.1H30.1c-.4 0-.8-.3-1-.7L26 9.5l-3.2 9.8c-.1.4-.5.7-1 .7H11.5l8.3 6.1c.4.3.5.7.4 1.1L17.1 37l8.3-6.1c.2-.1.4-.2.6-.2z";

let component = ReasonReact.reducerComponent("App");

let renderDesc = desc =>
  switch (desc) {
  | Some(str) => str
  | None => "No desciption :("
  };

let repoItems = (user, repos) =>
  repos
  |> List.map(repo =>
       <li
         key=(string_of_int(repo.id))
         className="flex flex-row justify-between w-full border-b">
         <div className="px-6 py-4">
           <div
             className="flex flex-row items-center font-mono text-base mb-2">
             <a
               href=repo.html_url
               target="_blank"
               className="no-underline text-pink-dark mr-2">
               (
                 ReasonReact.stringToElement(
                   repo.owner == user ? repo.name : repo.full_name,
                 )
               )
             </a>
             (
               repo.fork ?
                 <svg
                   className="fill-current text-green-dark h-4 w-4"
                   viewBox="0 0 10 16">
                   <path fillRule="evenodd" d=forkPath />
                 </svg> :
                 ReasonReact.nullElement
             )
           </div>
           <p className="font-mono text-pink-lighter text-xs">
             (repo.description |> renderDesc |> ReasonReact.stringToElement)
           </p>
         </div>
         <div className="flex flex-row items-center px-6 py-4">
           <span
             className="font-mono inline-block rounded-full px-1 py-1 text-sm text-pink-dark mr-1">
             (ReasonReact.stringToElement(string_of_int(repo.stars)))
           </span>
           <svg
             className="fill-current text-orange-light h-4 w-4 inline-block"
             viewBox="0 0 50 50">
             <path d=svgPath />
           </svg>
         </div>
       </li>
     );

let fetchRepos = ({ReasonReact.state, send}) => {
  Js.Promise.(
    Fetch.fetch(
      "https://api.github.com/users/"
      ++ state.username
      ++ "/repos?type=all&sort=updated",
    )
    |> then_(Fetch.Response.json)
    |> then_(json =>
         json
         |> Json.Decode.array(json =>
              Json.Decode.{
                id: json |> field("id", int),
                owner:
                  json
                  |> field("owner", owner => owner |> field("login", string)),
                name: json |> field("name", string),
                full_name: json |> field("full_name", string),
                stars: json |> field("stargazers_count", int),
                html_url: json |> field("html_url", string),
                description: json |> optional(field("description", string)),
                fork: json |> field("fork", bool),
              }
            )
         |> Array.to_list
         |> (
           repos => {
             send(ReposLoaded(repos));
             resolve();
           }
         )
       )
    |> ignore
  );
  send(Loading);
};

let make = _children => {
  ...component,
  initialState: () => {username: "", user: "", repos: RemoteData.NotAsked},
  reducer: (action, state) =>
    switch (action) {
    | ChangeUsername(username) => ReasonReact.Update({...state, username})
    | SearchEnterKeyDown =>
      ReasonReact.SideEffects((self => fetchRepos(self)))
    | Loading =>
      ReasonReact.Update({
        ...state,
        user: state.username,
        repos: RemoteData.Loading,
      })
    | ReposLoaded(repos) =>
      ReasonReact.Update({...state, repos: RemoteData.Success(repos)})
    | ReposError(err) =>
      ReasonReact.Update({...state, repos: RemoteData.Failure(err)})
    },
  render: ({state, send}) =>
    <div
      className="h-screen w-full bg-pink-lightest flex flex-col justify-start items-center overflow-scroll">
      <input
        className="mt-8 shadow border font-mono bg-yellow-lightest appearance-none py-2 px-3 text-pink rounded w-1/3"
        placeholder="Enter a Github Username . . ."
        value=state.username
        onKeyDown=(
          event =>
            if (ReactEvent.Keyboard.keyCode(event) === 13) {
              ReactEvent.Keyboard.preventDefault(event);
              send(SearchEnterKeyDown);
            }
        )
        onChange=(
          event =>
            send(ChangeUsername(ReactEvent.Form.target(event)##value))
        )
      />
      (
        switch (state.repos) {
        | NotAsked => ReasonReact.nullElement
        | Loading =>
          <p className="mt-8 font-mono text-pink text-lg">
            (ReasonReact.stringToElement("Loading..."))
          </p>
        | Failure(e) => <p> (ReasonReact.stringToElement(e)) </p>
        | Success(repos) =>
          if (List.length(repos) > 0) {
            <div
              className="bg-white shadow rounded flex overflow-scroll w-2/5 mb-8 mt-8">
              <ul
                className="appearance-none p-0 w-full text-grey-darker border rounded">
                (
                  ReasonReact.arrayToElement(
                    repos |> repoItems(state.user) |> Array.of_list,
                  )
                )
              </ul>
            </div>;
          } else {
            <p className="mt-8 font-mono text-pink text-lg">
              (
                ReasonReact.stringToElement(
                  state.user ++ " " ++ "does not have any public repos",
                )
              )
            </p>;
          }
        }
      )
    </div>,
};
