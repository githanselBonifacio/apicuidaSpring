package co.com.sura.postgres.repository.maestros.repository;

import co.com.sura.postgres.repository.maestros.data.ProfesionData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;

public interface ProfesionRepository extends ReactiveCrudRepository<ProfesionData,Integer> {
}
