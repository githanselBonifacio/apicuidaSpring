package co.com.sura.entity.remision;

import co.com.sura.entity.remision.historial.RegistroHistorialRemision;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public interface HistorialRemisionRepository {

    Flux<RegistroHistorialRemision> consultarHistoricoRemision(String idRemision);
    Mono<RegistroHistorialRemision> consultarDatosRemision(String idRemision);

}
