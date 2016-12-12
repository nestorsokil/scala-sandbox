package crudmock.client;

import crudmock.dao.UserDao$;
import crudmock.model.*;
import scala.Option;


import java.util.stream.Stream;

public class JavaClient {
    @SuppressWarnings("unchecked")
    public static void main(String[] args) {
        Stream.of(User$.MODULE$.apply("John Doe","johndoe@email.com", "abcd"))
                .forEach(UserDao$.MODULE$::create);
        Option<User> o = UserDao$.MODULE$.findById("1");
    }
}
