package co.com.sura.postgres.remision.repository;
import co.com.sura.postgres.remision.data.DiagnosticoData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;


public interface DiagnosticoRepository extends ReactiveCrudRepository<DiagnosticoData,String> {

}
