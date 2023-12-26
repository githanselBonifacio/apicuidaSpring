package co.com.sura.postgres.remision.repository.datospaciente;


import co.com.sura.postgres.remision.data.datospaciente.RemisionDiagnosticoData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

public interface RemisionDiagnosticoRepository extends ReactiveCrudRepository<RemisionDiagnosticoData,String> {

    @Query("INSERT INTO remision_diagnostico (codigo, id_remision, nombre_diagnostico) VALUES ($1, $2,$3);" )
    Mono<Void> insertDiagnosticoQuery(
            @Param("$1") String codigo,
            @Param("$2") String idRemision,
            @Param("$3") String nombreDiagnostico
    );

    @Transactional
    default Mono<Void> insertMultiplesDiagnosticos(List<RemisionDiagnosticoData> remisionDiagnosticoData) {
        Flux<RemisionDiagnosticoData> diagnosticoDataFlux = Flux.fromIterable(remisionDiagnosticoData);

        return diagnosticoDataFlux
                .flatMap(this::insertDiagnostico)
                .then();
    }

    default Mono<Void> insertDiagnostico(RemisionDiagnosticoData remisionDiagnosticoData){
        return  insertDiagnosticoQuery(
                remisionDiagnosticoData.getCodigo(),
                remisionDiagnosticoData.getIdRemision(),
                remisionDiagnosticoData.getNombreDiagnostico()

        );
    }

    @Query(" DELETE FROM remision_diagnostico WHERE id_remision = $1")
    Mono<Void> deleteByIdRemision(String idRemision);

    @Query("SELECT * FROM remision_diagnostico WHERE id_remision = $1")
    Flux<RemisionDiagnosticoData> findAllByIdRemision(String idRemision);

}
