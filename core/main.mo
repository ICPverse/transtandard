import Array "mo:base/Array";
import Nat "mo:base/Nat";
import Debug "mo:base/Debug";
import Hash "mo:base/Hash";
import Option "mo:base/Option";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import DIP "DIP721";
import IV "IV721"

actor class Landing(_owner: Principal) = this{
    private stable var globalPk: Nat = 0;
    private stable var collectionEntries : [(Text, Principal)] = [];
    private stable var collectionDIPCanisterEntries : [(Text, Text)] = [];
    private stable var collectionIVCanisterEntries : [(Text, Text)] = [];
    private stable var tokenNatureEntries : [(Nat, Text)] = [];
    private stable var localIdEntries : [(Nat, Nat)] = [];

    private let collections : HashMap.HashMap<Text, Principal> = HashMap.fromIter<Text, Principal>(collectionEntries.vals(), 10, Text.equal, Text.hash);
    private let collectionDIPCanisters : HashMap.HashMap<Text, Text> = HashMap.fromIter<Text, Text>(collectionDIPCanisterEntries.vals(), 10, Text.equal, Text.hash);
    private let collectionIVCanisters : HashMap.HashMap<Text, Text> = HashMap.fromIter<Text, Text>(collectionIVCanisterEntries.vals(), 10, Text.equal, Text.hash);
    private let tokenNatures : HashMap.HashMap<Nat, Text> = HashMap.fromIter<Nat, Text>(tokenNatureEntries.vals(), 10, Nat.equal, Hash.hash);
    private let localIds : HashMap.HashMap<Nat, Nat> = HashMap.fromIter<Nat, Nat>(localIdEntries.vals(), 10, Nat.equal, Hash.hash);   
    
    public type DRC721 = DIP.DRC721;
    public type IV721 = IV.DRC721;

    public shared({caller}) func requestApproval(collName: Text): async Bool{
        let creator = collections.get(collName);
        switch creator{
            case null{
                let res = collections.put(collName, caller);
                let status_dip = Option.get(collectionDIPCanisters.get(collName), "");
                let status_iv = Option.get(collectionIVCanisters.get(collName), "");
                if (status_dip != "" or status_iv != ""){
                    return false;
                };
                collectionDIPCanisters.put(collName, "pending");
                collectionIVCanisters.put(collName, "pending");
                
            };
            case (?principal){
                return false;
            };
        };
        return true;
    };

    public shared({caller}) func approveCollection(collName: Text): async Bool{
        if (caller != _owner){
            return false;
        };
        let creator = collections.get(collName);
        switch creator{
            case null{
                return false;
            };
            case (?principal){
                let status_dip = Option.get(collectionDIPCanisters.get(collName), "");
                let status_iv = Option.get(collectionIVCanisters.get(collName), "");
                if (status_dip != "pending" or status_iv != "pending"){
                    return false;
                };
                let _res1 = collectionDIPCanisters.replace(collName, "approved");
                let _res2 = collectionIVCanisters.replace(collName, "approved");
                return true;
            };
        };
        return false;

    };


    public shared({caller}) func launchCollection(collName: Text, symbol: Text, tags: [Text]): async (?DRC721, ?IV721){
        let creator = collections.get(collName);
        var creatorId = Principal.fromText("2vxsx-fae");
        switch creator{
            case null{
                return (null, null);
            };
            case (?principal){
                creatorId := principal;
                if (creatorId != caller){
                    return (null, null);
                };
            };
        };
        let status_dip = collectionDIPCanisters.get(collName);
        let status_iv = collectionIVCanisters.get(collName);
        switch (status_dip, status_iv){
            case (null, null){
                return (null, null);
            };
            case ((?text), null){
                return (null, null);
            };
            case (null, (?text)){
                return (null, null);
            };
            case ((?text1), (?text2)){
                if (text1 != "approved" or text2 != "approved"){
                    return (null, null);
                };
            };
        };
        let t = await DIP.DRC721(collName, symbol);
        let res = collectionDIPCanisters.replace(collName, Principal.toText(Principal.fromActor(t)));
        let t2 = await IV.DRC721(collName, symbol);
        let res2 = collectionIVCanisters.replace(collName, Principal.toText(Principal.fromActor(t2)));
        return (?t, ?t2);
    };

    public shared({caller}) func mint(collName: Text, uri: Text, meta: [Text], std: Text) : async Bool{
        assert(std == "dip" or std == "iv");
        let status_dip = Option.get(collectionDIPCanisters.get(collName), "");
        let status_iv = Option.get(collectionIVCanisters.get(collName), "");
        if (status_iv.size() < 10 or status_dip.size() < 10){
            return false;
        };
        
        
        if (std == "dip"){
            let act = actor(status_dip):actor {mint: (Text, [Text]) -> async (Nat)};
            let mintedNFT = await act.mint(uri, meta);
            let act2 = actor(status_dip):actor {transferFrom: (Principal, Principal, Nat) -> async ()};
            await act2.transferFrom(Principal.fromText("rrkah-fqaaa-aaaaa-aaaaq-cai"), caller, mintedNFT);
            globalPk += 1;
            tokenNatures.put(globalPk, "dip");
            localIds.put(globalPk, mintedNFT);
        }
        else {
            let act = actor(status_iv):actor {mint: (Text, [Text]) -> async (Nat)};
            let mintedNFT = await act.mint(uri, meta);
            let act2 = actor(status_iv):actor {transferFrom: (Principal, Principal, Nat) -> async ()};
            await act2.transferFrom(Principal.fromText("rrkah-fqaaa-aaaaa-aaaaq-cai"), caller, mintedNFT);
            globalPk += 1;
            tokenNatures.put(globalPk, "iv");
            localIds.put(globalPk, mintedNFT);
        };
        
        return true;
    };

    public shared({caller}) func switch_standard(collName: Text, id: Nat) : async Bool{
        
        let status_dip = Option.get(collectionDIPCanisters.get(collName), "");
        let status_iv = Option.get(collectionIVCanisters.get(collName), "");
        if (status_iv.size() < 10 or status_dip.size() < 10){
            return false;
        };
        
        let std = Option.get(tokenNatures.get(id), "");
        assert (std == "dip" or std == "iv");

        if (std == "dip"){
            let dip_id = Option.get(localIds.get(id), 0);
            assert(dip_id != 0);
            let act1 = actor(status_dip):actor {tokenURI: (Nat) -> async (?Text)};
            let uri = await act1.tokenURI(dip_id);
            let data1 = Option.get<Text>(uri, "");
            let act2 = actor(status_dip):actor {tokenMetadata: (Nat) -> async (?[Text])};
            let meta = await act2.tokenMetadata(dip_id);
            let data2 = Option.get<[Text]>(meta, []);
            assert (data1 != "");
            
            let act3 = actor(status_dip):actor {burn: (Nat) -> async ()};
            await act3.burn(dip_id);
            let act4 = actor(status_iv):actor {mint: (Text, [Text]) -> async (Nat)};
            let loc_id = await act4.mint(data1, data2);
            let _res1 = localIds.replace(id, loc_id);
            let _res2 = tokenNatures.replace(id, "iv");
            let act5 = actor(status_iv):actor {transferFrom: (Principal, Principal, Nat) -> async ()};
            await act5.transferFrom(Principal.fromText("rrkah-fqaaa-aaaaa-aaaaq-cai"), caller, loc_id);
            
        }
        else {
            let iv_id = Option.get(localIds.get(id), 0);
            assert(iv_id != 0);
            let act1 = actor(status_iv):actor {getTokenURI: (Nat) -> async (?Text)};
            let uri = await act1.getTokenURI(iv_id);
            let data1 = Option.get<Text>(uri, "");
            let act2 = actor(status_iv):actor {getTokenMetadata: (Nat) -> async (?[Text])};
            let meta = await act2.getTokenMetadata(iv_id);
            let data2 = Option.get<[Text]>(meta, []);
            assert (data1 != "");
            
            let act3 = actor(status_iv):actor {burn: (Nat) -> async ()};

            await act3.burn(iv_id);
            let act4 = actor(status_dip):actor {mint: (Text, [Text]) -> async (Nat)};
            let loc_id = await act4.mint(data1, data2);
            let _res1 = localIds.replace(id, loc_id);
            let _res2 = tokenNatures.replace(id, "dip");
            let act5 = actor(status_dip):actor {transferFrom: (Principal, Principal, Nat) -> async ()};
            await act5.transferFrom(Principal.fromText("rrkah-fqaaa-aaaaa-aaaaq-cai"), caller, loc_id);
        };
        
        return true;
    };

    
    public func getTokenDetails(collName: Text, id: Nat): async (Text, ?Text, ?[Text]) {
        let status_dip = Option.get(collectionDIPCanisters.get(collName), "");
        let status_iv = Option.get(collectionIVCanisters.get(collName), "");
        assert (status_iv.size() > 10 and status_dip.size() > 10);
        
        let std = Option.get(tokenNatures.get(id), "");
        assert (std == "dip" or std == "iv");
        let loc_id = Option.get(localIds.get(id), 0);
        assert (loc_id != 0);
        if (std == "dip"){
            let act = actor(status_dip):actor{tokenURI: (Nat) -> async (?Text)};
            let uri = await act.tokenURI(loc_id);
            let act2 = actor(status_dip):actor{tokenMetadata: (Nat) -> async (?[Text])};
            let meta = await act2.tokenMetadata(loc_id);
            return ("dip", uri, meta);
        }
        else {
            let act = actor(status_iv):actor{getTokenURI: (Nat) -> async (?Text)};
            let uri = await act.getTokenURI(loc_id);
            let act2 = actor(status_iv):actor{getTokenMetadata: (Nat) -> async (?[Text])};
            let meta = await act2.getTokenMetadata(loc_id);
            return ("iv", uri, meta);
        };
    };

    
        
       

    

    

    system func preupgrade(){
        collectionEntries := Iter.toArray(collections.entries());
        collectionDIPCanisterEntries := Iter.toArray(collectionDIPCanisters.entries());
        collectionIVCanisterEntries := Iter.toArray(collectionIVCanisters.entries());
        localIdEntries := Iter.toArray(localIds.entries());
        tokenNatureEntries := Iter.toArray(tokenNatures.entries());
    };

    system func postupgrade(){
        collectionEntries := [];
        collectionDIPCanisterEntries := [];
        collectionIVCanisterEntries := [];
        localIdEntries := [];
        tokenNatureEntries := [];
    };
};