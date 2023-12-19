package co.com.sura.postgres.remision.repository.datospaciente;
import co.com.sura.postgres.remision.data.datospaciente.DiagnosticoData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;


public interface DiagnosticoRepository extends ReactiveCrudRepository<DiagnosticoData,String> {

}
