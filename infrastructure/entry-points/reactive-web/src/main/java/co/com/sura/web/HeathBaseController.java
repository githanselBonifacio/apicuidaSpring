package co.com.sura.web;


import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

@RestController
public class HeathBaseController {


    @GetMapping(value = "/")
    public Mono<String> root(){
        return Mono.just("apicuida ok");
    }

    @GetMapping("/apicuida")
    public Mono<String> api(){
        return Mono.just("apicuida ok");
    }
}
