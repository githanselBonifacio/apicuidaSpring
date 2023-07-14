package co.com.sura.postgres.repository.remision.data;

import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Mono;


public interface DatosAtencionPacienteRepository extends ReactiveCrudRepository <DatosAtencionPacienteData,Integer> {

    Mono<DatosAtencionPacienteData> findAllByIdRemision(String idRemision);
}
